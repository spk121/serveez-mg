/*
 * ipdata.h - TCP/IP QueryEx definitons.
 *
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __IPDATA_H__
#define __IPDATA_H__ 1

/*
 * IP address entry.
 */
typedef struct IPAddrEntry
{
  unsigned long  iae_addr;
  unsigned long  iae_index;
  unsigned long  iae_mask;
  unsigned long  iae_bcastaddr;
  unsigned long  iae_reasmsize;
  unsigned short iae_context;
  unsigned short iae_pad;
}
IPAddrEntry;

#define IP_MIB_STATS_ID           1
#define IP_MIB_ADDRTABLE_ENTRY_ID 0x102
#define IP_INTFC_FLAG_P2P         1
#define IP_INTFC_INFO_ID          0x103
#define IF_MIB_STATS_ID           1
#define MAX_PHYSADDR_SIZE         8
#define MAX_IFDESCR_LEN           256

/*
 * Structure of an interface entry.
 */
typedef struct IFEntry
{
  unsigned long if_index;
  unsigned long if_type;
  unsigned long if_mtu;
  unsigned long if_speed;
  unsigned long if_physaddrlen;
  unsigned char if_physaddr[MAX_PHYSADDR_SIZE];
  unsigned long if_adminstatus;
  unsigned long if_operstatus;
  unsigned long if_lastchange;
  unsigned long if_inoctets;
  unsigned long if_inucastpkts;
  unsigned long if_innucastpkts;
  unsigned long if_indiscards;
  unsigned long if_inerrors;
  unsigned long if_inunknownprotos;
  unsigned long if_outoctets;
  unsigned long if_outucastpkts;
  unsigned long if_outnucastpkts;
  unsigned long if_outdiscards;
  unsigned long if_outerrors;
  unsigned long if_outqlen;
  unsigned long if_descrlen;
  unsigned char if_descr[1];
}
IFEntry;

/*
 * Structure of an entity ID.
 */
typedef struct TDIEntityID
{
  unsigned long tei_entity;
  unsigned long tei_instance;
}
TDIEntityID;

/*
 * Structure of an object ID.
 */
typedef struct TDIObjectID
{
  TDIEntityID   toi_entity;
  unsigned long toi_class;
  unsigned long toi_type;
  unsigned long toi_id;
}
TDIObjectID;

#define MAX_TDI_ENTITIES          512
#define INFO_CLASS_GENERIC        0x100
#define INFO_CLASS_PROTOCOL       0x200
#define INFO_CLASS_IMPLEMENTATION 0x300
#define INFO_TYPE_PROVIDER        0x100
#define INFO_TYPE_ADDRESS_OBJECT  0x200
#define INFO_TYPE_CONNECTION      0x300
#define ENTITY_LIST_ID            0
#define GENERIC_ENTITY            0
#define CL_NL_ENTITY              0x301
#define IF_ENTITY                 0x200
#define CONTEXT_SIZE              16

/*
 * The following are IDs supported by all entities.  They are of class
 * GENERIC and type PROVIDER.
 * The ID to get the entity type.  The return from this type is an
 * unsigned integer (see below).
 */
#define ENTITY_TYPE_ID 1

/*
 * Valid values to get back from entity type ID query.
 */
#define CL_NL_IP    0x303 /* Entity implements IP.  */
#define IF_GENERIC  0x200 /* Generic interface.  */
#define IF_MIB      0x202 /* Supports MIB-2 interface.  */

/*
 * QueryInformationEx IOCTL.  The return buffer is passed as the OutputBuffer
 * in the DeviceIoControl request.  This structure is passed as the
 * InputBuffer.
 */
struct tcp_request_query_information_ex
{
  TDIObjectID ID;                      /* object ID to query.  */
  unsigned char Context[CONTEXT_SIZE]; /* multi-request context.  Zeroed */
                                       /* for the first request.  */
};

typedef struct tcp_request_query_information_ex
   TCP_REQUEST_QUERY_INFORMATION_EX,
  *PTCP_REQUEST_QUERY_INFORMATION_EX;

#endif /* not __IPDATA_H__ */
