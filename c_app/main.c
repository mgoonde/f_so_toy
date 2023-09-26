#include <stddef.h>
#include <stdio.h>
#include <interf.h>

int main( int narg, char **arg){

  void *me =NULL;

  me = thing();

  int ierr;

  ierr = command(me, "some cmd");

  /* set an integer */
  int m;
  m = 9;
  set(me,"i",1,1, &m);

  /* set 1d int */
  int *i1d;
  int dim1=8;
  i1d = malloc( sizeof(int)*dim1);
  for ( int i = 0; i < dim1; i++){
    i1d[i] = i*3;
  }
  set( me, "int1d", dim1, 1, i1d);
  free(i1d);

  /* set 2d int */
  int *idata;
  int **i2d;
  dim1=2;
  int dim2=4;
  idata = malloc( sizeof(int)*dim1*dim2);
  i2d = malloc(sizeof(int)*dim2);
  int n = 0;
  for( int i = 0; i< dim2; i++){
    i2d[i] = &idata[n];
    n+= dim1;
  }
  n = 0;
  for( int i = 0; i< dim2; i++){
    idata[n+0] = 10*i;
    idata[n+1] = 10*i+1;
    n+= dim1;
  }
  i2d[3][1] =4;
  set( me, "int2d", dim1, dim2, *i2d );
  free(i2d);
  free(idata);


  /* set a float */
  float r;
  r=87.05;
  set(me, "r", 1, 1, &r);

  /* set 1d float */
  float *r1d;
  dim1 = 5;
  r1d=malloc(sizeof(float)*dim1);
  for( int j=0; j<dim1; j++)
    {
      r1d[j] = j + 0.2;
    }
  set( me, "real1d", dim1, 1, r1d );
  free(r1d);


  /* set 2d float */
  float *rdata;
  float **r2d;
  dim1 = 3;
  dim2 = 5;
  rdata=malloc(sizeof(float)*dim1*dim2);
  r2d=malloc(sizeof(float)*dim2);
  n=0;
  for( int i=0; i< dim2; i++){
    r2d[i] = &rdata[n];
    n+=dim1;
  }
  n=0;
  for( int i=0; i<dim2;i++){
    rdata[n+0] = 10.0*i + 1.1;
    rdata[n+1] = 10.0*i + 2.2;
    rdata[n+2] = 10.0*i + 3.3;
    n+=dim1;
  }
  r2d[3][2] = 4.1230;
  set( me, "real2d", dim1, dim2, *r2d);
  free(r2d);
  free(rdata);

  /* set string */
  set( me, "string", 1,1,"blablaBLAbla");

  /* call routine to print the current state */
  print(me);

  /* get int */
  int j;
  j = *(int *) get(me, "i");
  printf("got int:%d\n",j);

  /* get int1d */
  int *gi1d;
  int* csize;
  csize=malloc(sizeof(int)*2);
  csize = get_datasize(me, "int1d" );
  dim1 = csize[0];
  gi1d = (int *) get(me, "int1d" );
  printf("got i1d:");
  for( int i = 0; i< dim1; i++){
    printf("%d ",gi1d[i]);
  }
  printf("\n");




  /* get int2d as 1d array, then reshape */
  int *gi2d_data;
  int **gi2d;
  csize = get_datasize(me, "int2d");
  dim1 = csize[0];
  /* this is the 1d array wiht data */
  dim2 = csize[1];
  /* gi2d_data=malloc(sizeof(int)*dim1*dim2); */
  gi2d_data = (int *) get(me, "int2d");
  /* now reshape it */
  gi2d = malloc( sizeof(int)*dim2);
  n = 0;
  for (int i = 0; i< dim2; i++ ){
    gi2d[i] = &gi2d_data[n];
    n+=dim1;
  }

  printf("got int 2d:\n");
  for( int i = 0; i<dim2;i++){
    for( int j=0; j<dim1; j++){
      printf("%d ", gi2d[i][j]);
    }
    printf("\n");
  }
  free(gi2d);




  /* get float */
  float gr;
  gr = *(float *) get(me, "r");
  printf("got float: %f\n",gr);


  /* get float 1d */
  float *gr1d;
  csize = get_datasize(me, "real1d");
  dim1 = csize[0];
  gr1d = malloc(sizeof(float)*dim1);
  gr1d = (float *) get(me, "real1d");
  printf("got real 1d: ");
  for (int i = 0; i< dim1; i++){
    printf("%f ",gr1d[i]);
  }
  printf("\n");

  free(gr1d);


  /* get float2d as 1d array, then reshape */
  float *gr2d_data;
  float **gr2d;
  csize = get_datasize(me, "real2d");
  dim1 = csize[0];
  dim2 = csize[1];
  gr2d_data = (float *) get(me, "real2d");

  gr2d = malloc(sizeof(float)*dim2);
  n = 0;
  for( int i=0; i<dim2;i++){
    gr2d[i] = &gr2d_data[n];
    n+=dim1;
  }
  printf("got real 2d:\n");
  for( int i=0; i< dim2;i++){
    for(int j=0;j<dim1;j++){
      printf("%f ",gr2d[i][j]);
    }
    printf("\n");

  }
  free( gr2d);


  /* get string */
  char *s;
  s=(char *) get(me, "string");
  printf("got string: %s\n",s);
  /* free(csize); */


  close(me);

  return 0;
}
