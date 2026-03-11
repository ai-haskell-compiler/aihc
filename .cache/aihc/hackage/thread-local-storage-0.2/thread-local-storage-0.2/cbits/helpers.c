
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

long get_pthread_key_size() {
  return sizeof(pthread_key_t);
}

pthread_key_t easy_make_pthread_key() {
  pthread_key_t key;
  int rc = pthread_key_create(&key, NULL);
  if (rc) {
    fprintf(stderr, "pthread_key_create returned error code: %d\n", rc);
    abort();
  }
  return key;		     
}
