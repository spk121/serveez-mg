/*
 * mutex.c - thread mutex implementations
 *
 * Copyright (C) 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: mutex.c,v 1.2 2004/03/20 10:43:32 ela Exp $
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>

#include "util.h"
#include "mutex.h"

/* Creates and initializes the given @var{mutex} object.  The mutex is
   in an unlocked state.  The function must be called before using
   @code{svz_mutex_lock()} or @code{svz_mutex_unlock()}.  The user
   must call @code{svz_mutex_destroy()} for each mutex created by this
   function. */
int
svz_mutex_create (svz_mutex_t *mutex)
{
  return pthread_mutex_init (mutex, NULL);
}

/* Destroys the given @var{mutex} object which has been created by
   @code{svz_mutex_create()}. */
int
svz_mutex_destroy (svz_mutex_t *mutex)
{
  if (pthread_mutex_destroy (mutex) != 0)
    {
      svz_log (LOG_ERROR, "pthread_mutex_destroy: %s\n", SYS_ERROR);
      return -1;
    }
  return 0;
}

/* Locks a @var{mutex} object and sets the current thread into an idle
   state if the @var{mutex} object has been currently locked by another
   thread. */
int
svz_mutex_lock (svz_mutex_t *mutex)
{
  if (errno != EINTR && pthread_mutex_lock (mutex) != 0)
    {
      fprintf (stderr, "pthread_mutex_lock: %s\n", SYS_ERROR);
      return -1;
    }
  return 0;
}

/* Releases the given @var{mutex} object and thereby possibly resumes
   a waiting thread calling @code{svz_mutex_lock()}. */
int
svz_mutex_unlock (svz_mutex_t *mutex)
{
  if (pthread_mutex_unlock (mutex) != 0)
    {
      svz_log (LOG_ERROR, "pthread_mutex_unlock: %s\n", SYS_ERROR);
      return -1;
    }
  return 0;
}
