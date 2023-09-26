#include <stdlib.h>

void *api_thing_open();
int api_command( void* handle, const char *cmd);
void *api_get_thing( void* handle, const char *cmd);
void *api_set_thing( void* handle, const char *name, int **size, void **r );
void *api_print_thing( void* handle );
int api_get_datatype( void* handle, const char *name);
int *api_get_datasize( void *handle, const char *name);
void api_thing_close( void* handle );



void *thing(){
  return api_thing_open();
}

void close( void* handle ){
  return api_thing_close(handle);
}

int command( void *handle, const char *cmd){
  return api_command( handle, cmd );
}


void *get( void* handle, const char *cmd){
  return api_get_thing( handle, cmd);
}

int *get_datasize( void* handle, const char *name){
  return api_get_datasize(handle, name);
}

void *set( void* handle, const char *name, int dim1, int dim2, void *r ){
  int *csize;
  csize=malloc(sizeof(int)*2);
  csize[0] = dim1; csize[1] = dim2;
  return api_set_thing( handle, name, &csize, &r);
  free(csize);
}

void *print( void* handle ){
  return api_print_thing( handle );
}

