#ifndef __CHANNELS_H_
#define __CHANNELS_H_

#include <pthread.h>
#include <stdint.h>

typedef struct Node {
  int64_t *value;
  struct Node *next;
  struct Node *prev;
} node_ty;

typedef struct Queue {
  node_ty *head;
  node_ty *tail;
} queue_ty;

typedef struct Channel {
  queue_ty q;
  pthread_mutex_t lock;
  pthread_cond_t available;
} channel_ty;

int64_t *chan_create();
int64_t chan_send(channel_ty *handle, int64_t *val);
int64_t *chan_recv(channel_ty *handle);
int64_t thread_spawn(void *fptr(void *), void *args);
int64_t thread_join(int64_t tid);

#endif // __CHANNELS_H_
