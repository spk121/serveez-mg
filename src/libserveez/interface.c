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

#include <config.h>

#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/core.h"
#include "libserveez/vector.h"
#include "libserveez/interface.h"

/*
 * The available interface list.
 */
svz_vector_t *svz_interfaces = NULL;


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
      if (ifr->ifr_addr.sa_family != AF_INET)
	continue;

      strcpy (ifr2.ifr_name, ifr->ifr_name);
      ifr2.ifr_addr.sa_family = AF_INET;
    }
  
  close (fd);
  svz_free (ifc.ifc_buf);
}

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
