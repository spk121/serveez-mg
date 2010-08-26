/*
 * control-proto.h - control protocol header definitions
 *
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 *
 * $Id: control-proto.h,v 1.13 2002/07/31 20:21:32 ela Exp $
 *
 */

#ifndef __CONTROL_PROTO_H__
#define __CONTROL_PROTO_H__

#include <config.h>

#define STAT_BUFFER_SIZE 256

/*
 * System statistics structure.
 */
typedef struct
{
  char *cpufile;                /* CPU state file under Linux */
  char *cpuline;                /* CPU line format under Linux */
  char *cpuinfoline;            /* the info format string */
  char info[STAT_BUFFER_SIZE];  /* the info line itself */
  char pinfo[STAT_BUFFER_SIZE]; /* process info itself */
  unsigned long cpu[2][4];      /* cpu values */
  unsigned long total[2];
  unsigned long proc[2][4];     /* process values */
  unsigned long ptotal[2];
  int index;                    /* index for cpu differences */
}
cpu_state_t;

/*
 * Control protocol server configuration.
 */
typedef struct
{
  int nothing; /* (yet) */
}
ctrl_config_t;

/* Export the control server definition. */
extern svz_servertype_t ctrl_server_definition;

/* server functions */
int ctrl_init (svz_server_t *server);
int ctrl_finalize (svz_server_t *server);
char *ctrl_info_server (svz_server_t *server);
char *ctrl_info_client (svz_server_t *server, svz_socket_t *sock);

/* basic protocol functions */
int ctrl_detect_proto (svz_server_t *server, svz_socket_t *sock);
int ctrl_connect_socket (svz_server_t *server, svz_socket_t *sock);

int ctrl_idle (svz_socket_t *sock);
int ctrl_handle_request (svz_socket_t *sock, char *request, int len);

#define CTRL_FLAG_PASSED          0x0001
#define CTRL_PACKET_DELIMITER     "\n"
#define CTRL_PACKET_DELIMITER_LEN 1

/*
 * Format string for system business output on different systems.
 */
# define CPU_FORMAT \
  "user %ld.%01ld%%, nice %ld.%01ld%%, sys %ld.%01ld%%, idle %ld.%01ld%%"

#define PROC_FORMAT \
  "user %ld.%01ld%%, sys %ld.%01ld%%, " \
  "child user %ld.%01ld%%, child sys %ld.%01ld%%"

#define CPU_FILE_NAME   "/proc/stat"
#define CPU_LINE_FORMAT "cpu  %lu %lu %lu %lu"

/* welcome message */
#define CTRL_PASSWD "Welcome to serveez control center. " \
                    "Please login.\r\n\r\n" \
                    "Password: "

/* the control protocol client prompt */
#define CTRL_PROMPT "ctrl-sh $ "

/* the receive and send buffer size of an control protocol client */
#define CTRL_RECV_BUFSIZE 512
#define CTRL_SEND_BUFSIZE 1024 * 100

/* how often we update the CPU information (in seconds) */
#define CTRL_LOAD_UPDATE 1

/* these are all available instructions */
#define CTRL_CMD_HELP          "help"
#define CTRL_CMD_QUIT          "quit"
#define CTRL_CMD_EXIT          "exit"
#define CTRL_CMD_BYE           "bye"
#define CTRL_CMD_STAT          "stat"
#define CTRL_CMD_STAT_CON      "stat con"
#define CTRL_CMD_STAT_ALL      "stat all"
#define CTRL_CMD_STAT_ID       "stat id"
#define CTRL_CMD_STAT_COSERVER "stat coserver"
#define CTRL_CMD_STAT_CACHE    "stat cache"
#define CTRL_CMD_KILL_CACHE    "kill cache"
#define CTRL_CMD_KILLALL       "killall"
#define CTRL_CMD_KILL_ID       "kill id"
#define CTRL_CMD_RESTART_RDNS  "restart reverse dns"
#define CTRL_CMD_RESTART_DNS   "restart dns"
#define CTRL_CMD_RESTART_IDENT "restart ident"

#endif /* __CONTROL_PROTO_H__ */
