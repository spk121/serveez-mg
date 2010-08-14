/*
 * interfaces.c - network interface function implementation
 *
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * $Id: interface.c,v 1.14 2002/10/13 08:11:59 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if ENABLE_IFLIST

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <net/if.h>
# include <netinet/in.h>
# include <arpa/inet.h>
#endif

#if HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

/* Solaris, IRIX */
#if HAVE_SYS_SOCKIO_H
# include <sys/sockio.h>
#endif

#ifdef __MINGW32__
# include <winsock2.h>
# include "libserveez/ipdata.h" 
# include "libserveez/iphlpapi.h"
#endif

#endif /* ENABLE_IFLIST */

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/core.h"
#include "libserveez/vector.h"
#include "libserveez/interface.h"

/*
 * The available interface list.
 */
svz_vector_t *svz_interfaces = NULL;

#if ENABLE_IFLIST

#ifdef __MINGW32__

/* Function pointer definition for use with GetProcAddress. */
typedef int (__stdcall *WsControlProc) (DWORD, DWORD, LPVOID, LPDWORD,
					LPVOID, LPDWORD);
#define WSCTL_TCP_QUERY_INFORMATION 0
#define WSCTL_TCP_SET_INFORMATION   1   

/*
 * The local interface list is requested by some "unrevealed" Winsock API 
 * routine called "WsControl". Works with Win95 and Win98.
 * Otherwise try using the IP Helper API which works with WinNT4x and Win2k.
 */
static WsControlProc WsControl = NULL;
static GetIfTableProc GetIfTable = NULL;
static GetIpAddrTableProc GetIpAddrTable = NULL;

#define NO_METHOD    0
#define WSCTL_METHOD 1
#define IPAPI_METHOD 2

void
svz_interface_collect (void)
{
  int result = 0;
  HMODULE WSockHandle;
  WSADATA WSAData;
  TCP_REQUEST_QUERY_INFORMATION_EX tcpRequestQueryInfoEx;
  DWORD tcpRequestBufSize;
  DWORD entityIdsBufSize;
  TDIEntityID *entityIds;
  DWORD entityCount;
  DWORD i, n, k;
  DWORD ifCount;
  ULONG entityType;
  DWORD entityTypeSize;
  DWORD ifEntrySize;
  IFEntry *ifEntry;
  DWORD ipAddrEntryBufSize;
  IPAddrEntry *ipAddrEntry;
  ULONG ifTableSize, ipTableSize;
  PMIB_IFTABLE ifTable;
  PMIB_IPADDRTABLE ipTable;
  unsigned long addr;
  svz_interface_t *ifc;

  DWORD Method = NO_METHOD;

  /*
   * Try getting WsControl () from "wsock32.dll" via LoadLibrary
   * and GetProcAddress. Or try the IP Helper API.
   */
  if ((WSockHandle = LoadLibrary ("iphlpapi.dll")) != NULL)
    {
      Method = IPAPI_METHOD;
    }
  else
    {
      if ((WSockHandle = LoadLibrary ("wsock32.dll")) != NULL)
	{
	  WsControl = (WsControlProc) 
	    GetProcAddress (WSockHandle, "WsControl");
	  if (!WsControl)
	    {
	      printf ("GetProcAddress (WsControl): %s\n", SYS_ERROR);
	      FreeLibrary (WSockHandle);
	      return;
	    }
	  Method = WSCTL_METHOD;
	}
      else
	{
	  printf ("LoadLibrary (WSock32.dll): %s\n", SYS_ERROR);
	  return;
	}
    }

  if (Method == WSCTL_METHOD)
    {
      result = WSAStartup (MAKEWORD (1, 1), &WSAData);
      if (result) 
	{
	  printf ("WSAStartup: %s\n", NET_ERROR);
	  FreeLibrary (WSockHandle);
	  return;
	}

      memset (&tcpRequestQueryInfoEx, 0, sizeof (tcpRequestQueryInfoEx));
      tcpRequestQueryInfoEx.ID.toi_entity.tei_entity = GENERIC_ENTITY;
      tcpRequestQueryInfoEx.ID.toi_entity.tei_instance = 0;
      tcpRequestQueryInfoEx.ID.toi_class = INFO_CLASS_GENERIC;
      tcpRequestQueryInfoEx.ID.toi_type = INFO_TYPE_PROVIDER;
      tcpRequestQueryInfoEx.ID.toi_id = ENTITY_LIST_ID;
      tcpRequestBufSize = sizeof (tcpRequestQueryInfoEx);

      /*
       * this probably allocates too much space; not sure if MAX_TDI_ENTITIES
       * represents the max number of entities that can be returned or, if it
       * is the highest entity value that can be defined.
       */
      entityIdsBufSize = MAX_TDI_ENTITIES * sizeof (TDIEntityID);
      entityIds = (TDIEntityID *) calloc (1, entityIdsBufSize);
      
      result = WsControl (IPPROTO_TCP,
			  WSCTL_TCP_QUERY_INFORMATION,
			  &tcpRequestQueryInfoEx,
			  &tcpRequestBufSize, entityIds, &entityIdsBufSize);
      
      if (result) 
	{
	  printf ("WsControl: %s\n", NET_ERROR);
	  WSACleanup ();
	  FreeLibrary (WSockHandle);
	  free (entityIds);
	  return;
	}

      /* ... after the call we compute */
      entityCount = entityIdsBufSize / sizeof (TDIEntityID);
      ifCount = 0;

      /* find out the interface info for the generic interfaces */
      for (i = 0; i < entityCount; i++) 
	{
	  if (entityIds[i].tei_entity == IF_ENTITY) 
	    {
	      ++ifCount;

	      /* see if the interface supports snmp mib-2 info */
	      memset (&tcpRequestQueryInfoEx, 0,
		      sizeof (tcpRequestQueryInfoEx));
	      tcpRequestQueryInfoEx.ID.toi_entity = entityIds[i];
	      tcpRequestQueryInfoEx.ID.toi_class = INFO_CLASS_GENERIC;
	      tcpRequestQueryInfoEx.ID.toi_type = INFO_TYPE_PROVIDER;
	      tcpRequestQueryInfoEx.ID.toi_id = ENTITY_TYPE_ID;

	      entityTypeSize = sizeof (entityType);
	      
	      result = WsControl (IPPROTO_TCP,
				  WSCTL_TCP_QUERY_INFORMATION,
				  &tcpRequestQueryInfoEx,
				  &tcpRequestBufSize,
				  &entityType, &entityTypeSize);
	      
	      if (result) 
		{
		  printf ("WsControl: %s\n", NET_ERROR);
		  WSACleanup ();
		  FreeLibrary (WSockHandle);
		  free (entityIds);
		  return;
		}

	      if (entityType == IF_MIB) 
		{ 
		  /* Supports MIB-2 interface. Get snmp mib-2 info. */
		  tcpRequestQueryInfoEx.ID.toi_class = INFO_CLASS_PROTOCOL;
		  tcpRequestQueryInfoEx.ID.toi_id = IF_MIB_STATS_ID;

		  /*
		   * note: win95 winipcfg use 130 for MAX_IFDESCR_LEN while
		   * ddk\src\network\wshsmple\SMPLETCP.H defines it as 256
		   * we are trying to dup the winipcfg parameters for now
		   */
		  ifEntrySize = sizeof (IFEntry) + 128 + 1;
		  ifEntry = (IFEntry *) calloc (ifEntrySize, 1);
		  
		  result = WsControl (IPPROTO_TCP,
				      WSCTL_TCP_QUERY_INFORMATION,
				      &tcpRequestQueryInfoEx,
				      &tcpRequestBufSize,
				      ifEntry, &ifEntrySize);

		  if (result) 
		    {
		      printf ("WsControl: %s\n", NET_ERROR);
		      WSACleanup ();
		      FreeLibrary (WSockHandle);
		      return;
		    }

		  /* store interface index and description */
		  *(ifEntry->if_descr + ifEntry->if_descrlen) = '\0';
		  svz_interface_add (ifEntry->if_index, 
				     (char *) ifEntry->if_descr,
				     ifEntry->if_index, 1);
		}
	    }
	}
  
      /* find the ip interfaces */
      for (i = 0; i < entityCount; i++) 
	{
	  if (entityIds[i].tei_entity == CL_NL_ENTITY) 
	    {
	      /* get ip interface info */
	      memset (&tcpRequestQueryInfoEx, 0,
		      sizeof (tcpRequestQueryInfoEx));
	      tcpRequestQueryInfoEx.ID.toi_entity = entityIds[i];
	      tcpRequestQueryInfoEx.ID.toi_class = INFO_CLASS_GENERIC;
	      tcpRequestQueryInfoEx.ID.toi_type = INFO_TYPE_PROVIDER;
	      tcpRequestQueryInfoEx.ID.toi_id = ENTITY_TYPE_ID;

	      entityTypeSize = sizeof (entityType);

	      result = WsControl (IPPROTO_TCP,
				  WSCTL_TCP_QUERY_INFORMATION,
				  &tcpRequestQueryInfoEx,
				  &tcpRequestBufSize,
				  &entityType, &entityTypeSize);

	      if (result) 
		{
		  printf ("WsControl: %s\n", NET_ERROR);
		  WSACleanup ();
		  FreeLibrary (WSockHandle);
		  return;
		}

	      if (entityType == CL_NL_IP) 
		{
		  /* Entity implements IP. Get ip address list. */
		  tcpRequestQueryInfoEx.ID.toi_class = INFO_CLASS_PROTOCOL;
		  tcpRequestQueryInfoEx.ID.toi_id = IP_MIB_ADDRTABLE_ENTRY_ID;

		  ipAddrEntryBufSize = sizeof (IPAddrEntry) * ifCount;
		  ipAddrEntry = 
		    (IPAddrEntry *) calloc (ipAddrEntryBufSize, 1);

		  result = WsControl (IPPROTO_TCP,
				      WSCTL_TCP_QUERY_INFORMATION,
				      &tcpRequestQueryInfoEx,
				      &tcpRequestBufSize,
				      ipAddrEntry, &ipAddrEntryBufSize);

		  if (result) 
		    {
		      printf ("WsControl: %s\n", NET_ERROR);
		      WSACleanup ();
		      FreeLibrary (WSockHandle);
		      return;
		    }
		
		  /* find ip address list and interface description */
		  for (n = 0; n < ifCount; n++) 
		    {
		      memcpy (&addr, &ipAddrEntry[n].iae_addr, sizeof (addr));

		      for (k = 0; k < svz_vector_length (svz_interfaces); k++)
			{
			  ifc = svz_vector_get (svz_interfaces, k);
			  if (ifc->index == ipAddrEntry[n].iae_index)
			    ifc->ipaddr = addr;
			}
		    }
		}
	    }
	}

      WSACleanup ();
      FreeLibrary (WSockHandle);
    }

  /* this is for WinNT... */
  else if (Method == IPAPI_METHOD)
    {
      /* Use of the IPHelper-API here. */
      GetIfTable = (GetIfTableProc) 
	GetProcAddress (WSockHandle, "GetIfTable");
      if (!GetIfTable)
	{
	  printf ("GetProcAddress (GetIfTable): %s\n", SYS_ERROR);
	  FreeLibrary (WSockHandle);
	  return;
	}

      GetIpAddrTable = (GetIpAddrTableProc) 
	GetProcAddress (WSockHandle, "GetIpAddrTable");
      if (!GetIpAddrTable)
	{
	  printf ("GetProcAddress (GetIpAddrTable): %s\n", SYS_ERROR);
	  FreeLibrary (WSockHandle);
	  return;
	}

      ifTableSize = sizeof (MIB_IFTABLE);
      ifTable = (PMIB_IFTABLE) svz_malloc (ifTableSize);
      GetIfTable (ifTable, &ifTableSize, FALSE);
      ifTable = (PMIB_IFTABLE) svz_realloc (ifTable, ifTableSize);
      if (GetIfTable (ifTable, &ifTableSize, FALSE) != NO_ERROR)
	{
	  printf ("GetIfTable: %s\n", SYS_ERROR);
	  FreeLibrary (WSockHandle);
	  svz_free (ifTable);
	  return;
	}
  
      ipTableSize = sizeof (MIB_IPADDRTABLE);
      ipTable = (PMIB_IPADDRTABLE) svz_malloc (ipTableSize);
      GetIpAddrTable (ipTable, &ipTableSize, FALSE);
      ipTable = (PMIB_IPADDRTABLE) svz_realloc (ipTable, ipTableSize);
      if (GetIpAddrTable (ipTable, &ipTableSize, FALSE) != NO_ERROR)
	{
	  printf ("GetIpAddrTable: %s\n", SYS_ERROR);
	  FreeLibrary (WSockHandle);
	  svz_free (ipTable);
	  svz_free (ifTable);
	  return;
	}
      
      for (n = 0; n < ipTable->dwNumEntries; n++)
	{
	  for (i = 0; i < ifTable->dwNumEntries; i++)
	    {
	      if (ifTable->table[i].dwIndex == ipTable->table[n].dwIndex)
		{
		  ifTable->table[i].bDescr[ifTable->table[i].dwDescrLen] = 0;
		  svz_interface_add (ipTable->table[n].dwIndex, 
				     (char *) ifTable->table[i].bDescr,
				     ipTable->table[n].dwAddr, 1);
		  break;
		}
	    }
	  if (i == ifTable->dwNumEntries)
	    {
	      svz_interface_add (ipTable->table[n].dwIndex, NULL,
				 ipTable->table[n].dwAddr, 1);
	    }
	}

      svz_free (ipTable);
      svz_free (ifTable);
      FreeLibrary (WSockHandle);
    }
  else
    {
      printf ("Neither IPHlpApi.dll nor WSock32.WsControl found...\n");
    }
}

#else /* not __MINGW32__ */

/*
 * Collect all available network interfaces and put them into the list
 * @var{svz_interfaces}. This is useful in order to @code{bind()} server
 * sockets to specific network interfaces. Thus you can make certain 
 * services accessible from "outside" or "inside" a network installation
 * only.
 */
void
svz_interface_collect (void)
{
  int numreqs = 16;
  struct ifconf ifc;
  struct ifreq *ifr;
  struct ifreq ifr2;
  int n;
  int fd;

  /* Get a socket out of the Internet Address Family. */
  if ((fd = socket (AF_INET, SOCK_STREAM, 0)) < 0) 
    {
      perror ("socket");
      return;
    }

  /* Collect information. */
  ifc.ifc_buf = NULL;
  for (;;) 
    {
      ifc.ifc_len = sizeof (struct ifreq) * numreqs;
      ifc.ifc_buf = svz_realloc (ifc.ifc_buf, ifc.ifc_len);

      /*
       * On newer AIXes we cannot use SIOCGICONF anymore, although it is
       * present. The data structure returned is bogus. Using OSIOCGIFCONF.
       */
#if defined (OSIOCGIFCONF)
      if (ioctl (fd, OSIOCGIFCONF, &ifc) < 0)
	{
	  perror ("OSIOCGIFCONF");
	  close (fd);
	  svz_free (ifc.ifc_buf);
	  return;	  
	}
#else /* OSIOCGIFCONF */
      if (ioctl (fd, SIOCGIFCONF, &ifc) < 0) 
	{
	  perror ("SIOCGIFCONF");
	  close (fd);
	  svz_free (ifc.ifc_buf);
	  return;
	}
#endif /* OSIOCGIFCONF */

      if ((unsigned) ifc.ifc_len == sizeof (struct ifreq) * numreqs) 
	{
	  /* Assume it overflowed and try again. */
	  numreqs += 10;
	  continue;
	}
      break;
    }

  ifr = ifc.ifc_req;
  for (n = 0; n < ifc.ifc_len; n += sizeof (struct ifreq), ifr++)
    {
      /*
       * On AIX (and perhaps others) you get interfaces that are not AF_INET
       * from the first `ioctl ()', so filter here again.
       */
#if defined (__FreeBSD__)
      if ((ifr->ifr_phys & 0xFFFF0000) == 0)
#elif defined (__NetBSD__) || defined (__OpenBSD__)
      if (((long) ifr->ifr_data & 0xFFFF0000) != 0)
#else
      if (ifr->ifr_addr.sa_family != AF_INET)
#endif
	continue;

      strcpy (ifr2.ifr_name, ifr->ifr_name);
      ifr2.ifr_addr.sa_family = AF_INET;
      if (ioctl (fd, SIOCGIFADDR, &ifr2) == 0)
	{
	  static int index = 0;

	  /* 
	   * The following cast looks bogus. ifr2.ifr_addr is a
	   * (struct sockaddr), but we know that we deal with a 
	   * (struct sockaddr_in) here. Since you cannot cast structures
	   * in C, I cast addresses just to get a (struct sockaddr_in) in 
	   * the end ...
	   */
#ifdef ifr_ifindex
	  index = ifr->ifr_ifindex;
#else
	  index++;
#endif
	  svz_interface_add (index, ifr->ifr_name, 
			     (*(struct sockaddr_in *) 
			      (void *) &ifr2.ifr_addr).sin_addr.s_addr, 1);
	}
    }
  
  close (fd);
  svz_free (ifc.ifc_buf);
}

#endif /* not __MINGW32__ */

#else /* not ENABLE_IFLIST */

void
svz_interface_collect (void)
{
  printf ("\n"
	  "Sorry, the list of local interfaces is not available. If you\n"
	  "know how to get such a list on your OS, please contact\n"
	  "Raimund Jacob <raimi@lkcc.org>. Thanks.\n\n");
}

#endif /* not ENABLE_IFLIST */

/*
 * Print the text representation of all the network interfaces.
 */
void
svz_interface_list (void)
{
  unsigned long n;
  svz_interface_t *ifc;

  printf ("--- list of local interfaces you can start ip services on ---\n");

  /* any interfaces at all ? */
  if (!svz_interfaces)
    return;

  for (n = 0; n < svz_vector_length (svz_interfaces); n++)
    {
      ifc = svz_vector_get (svz_interfaces, n);

      /* interface with description */
      if (ifc->description)
	{
	  printf ("%40s: %s\n", ifc->description, 
		  svz_inet_ntoa (ifc->ipaddr));
	}
      else
	{
	  /* interface with interface # only */
	  printf ("%31s%09lu: %s\n", "interface # ",
		  ifc->index, svz_inet_ntoa (ifc->ipaddr));
	}
    }
}

/*
 * Add a network interface to the current list of known interfaces. Drop
 * duplicate entries. The given arguments @var{index} specifies the network
 * interface index number, @var{desc} an interface desription, @var{addr}
 * the IP address in network byte order and the @var{detected} flag if
 * the given network interface has been detected by Serveez itself or not.
 */
int
svz_interface_add (int index, char *desc, unsigned long addr, int detected)
{
  char *p;
  unsigned long n;
  svz_interface_t *ifc;

  /* Check if there is such an interface already. */
  if (svz_interfaces == NULL)
    {
      svz_interfaces = svz_vector_create (sizeof (svz_interface_t));
    }
  else
    {
      for (n = 0; n < svz_vector_length (svz_interfaces); n++)
	{
	  ifc = svz_vector_get (svz_interfaces, n);
	  if (ifc->ipaddr == addr)
	    return -1;
	}
    }

  /* Actually add this interface. */
  ifc = svz_malloc (sizeof (svz_interface_t));
  ifc->detected = detected ? 1 : 0;
  ifc->index = index;
  ifc->ipaddr = addr;
  ifc->description = svz_strdup (desc);

  /* Delete trailing white space characters. */
  p = ifc->description + strlen (ifc->description) - 1;
  while (p > ifc->description && 
	 (*p == '\n' || *p == '\r' || *p == '\t' || *p == ' '))
    *p-- = '\0';

  svz_vector_add (svz_interfaces, ifc);
  svz_free (ifc);
  return 0;
}

/*
 * This function returns the interface structure for the given IP address
 * @var{addr} if any. Returns @code{NULL} otherwise.
 */
svz_interface_t *
svz_interface_get (unsigned long addr)
{
  svz_interface_t *ifc;
  int n;

  svz_vector_foreach (svz_interfaces, ifc, n)
    {
      if (ifc->ipaddr == addr)
	return ifc;
    }
  return NULL;
}

/*
 * The following function returns a network interface structure for a given
 * interface name (e.g. eth0). If no such interface exists it returns 
 * @code{NULL}.
 */
svz_interface_t *
svz_interface_search (char *desc)
{
  svz_interface_t *ifc;
  int n;

  svz_vector_foreach (svz_interfaces, ifc, n)
    if (!strcmp (ifc->description, desc))
      return ifc;
  return NULL;
}

/*
 * Free the network interface list.
 */
int
svz_interface_free (void)
{
  unsigned long n;
  svz_interface_t *ifc;

  if (svz_interfaces)
    {
      svz_vector_foreach (svz_interfaces, ifc, n)
	{
	  if (ifc->description)
	    svz_free (ifc->description);
	}
      svz_vector_destroy (svz_interfaces);
      svz_interfaces = NULL;
      return 0;
    }
  return -1;
}

/*
 * This function checks for network interface changes. It emits messages for
 * new and removed interfaces. Software interfaces which have not been 
 * detected by Serveez stay untouched. If Serveez receives a @code{SIGHUP} 
 * signal the signal handler runs it once.
 */
void
svz_interface_check (void)
{
  svz_vector_t *interfaces = NULL;
  svz_interface_t *ofc, *ifc;
  int o, n, found, changes = 0;

  if (svz_interfaces)
    {
      /* Save old interface list. */
      interfaces = svz_interfaces;
      svz_interfaces = NULL;
      svz_interface_collect ();

      /* Look for removed network interfaces. */
      svz_vector_foreach (interfaces, ifc, n)
	{
	  if (svz_interface_get (ifc->ipaddr) == NULL)
	    {
	      if (!ifc->detected)
		{
		  /* Re-apply software network interfaces. */
		  svz_interface_add (ifc->index, ifc->description, 
				     ifc->ipaddr, ifc->detected);
		}
	      else
		{
		  svz_log (LOG_NOTICE, "%s: %s has been removed\n",
			   ifc->description, svz_inet_ntoa (ifc->ipaddr));
		  changes++;
		}
	    }
	}

      /* Look for new network interfaces. */
      svz_vector_foreach (svz_interfaces, ifc, n)
	{
	  found = 0;
	  svz_vector_foreach (interfaces, ofc, o)
	    {
	      if (ofc->ipaddr == ifc->ipaddr)
		found++;
	    }
	  if (!found)
	    {
	      svz_log (LOG_NOTICE, "%s: %s has been added\n",
		       ifc->description, svz_inet_ntoa (ifc->ipaddr));
	      changes++;
	    }
	}

      /* Destroy old interface list and apply new interface list. */
      svz_vector_foreach (interfaces, ifc, n)
	if (ifc->description)
	  svz_free (ifc->description);
      svz_vector_destroy (interfaces);
    }

  /* Print a notification message if no changes occurred. */
  if (!changes)
    {
      svz_log (LOG_NOTICE, "no network interface changes detected\n");
    }
}
