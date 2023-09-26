from ctypes import *
import numpy as np
from os.path import dirname,abspath,join
from inspect import getsourcefile

class thing():

    # values from src/thing_datatype.f90
    _THING_DATA_UNKNOWN = 0
    _THING_DATA_INT     = 1
    _THING_DATA_INT_1D  = 2
    _THING_DATA_INT_2D  = 3
    _THING_DATA_REAL    = 4
    _THING_DATA_REAL_1D = 5
    _THING_DATA_REAL_2D = 6
    _THING_DATA_STR     = 7

    def __init__(self, shlib=None):
        # load the lib.so

        # path to this file
        mypath=dirname(abspath(getsourcefile(lambda:0)))
        # one directory up
        mypath=dirname(mypath)
        # by default, the lib should be there:
        path = join(mypath,"lib/libthemodule.so")

        if shlib:
            # user provde path
            path=shlib

        self.lib = CDLL(path)

        # get the handle
        self.lib.api_thing_open.restype = c_void_p
        self.handle = c_void_p( self.lib.api_thing_open() )


        # some common argtypes and restypes
        self.lib.api_thing_close.restype = None
        self.lib.api_thing_close.argtypes = [ c_void_p ]

        self.lib.api_print_thing.restype = None
        self.lib.api_print_thing.argtypes = [ c_void_p ]

        self.lib.api_get_datatype.restype = c_int
        self.lib.api_get_datatype.argtypes = [ c_void_p, c_char_p ]

        self.lib.api_set_thing.restype = None

        self.lib.api_get_datasize.restype = POINTER(c_int)
        self.lib.api_get_datasize.argtypes = [ c_void_p, c_char_p ]

        self.lib.api_get_thing.argtypes = [ c_void_p, c_char_p ]

        self.lib.api_command.restype = c_int
        self.lib.api_command.argtypes = [ c_void_p, c_char_p ]


    def close( self ):
        self.lib.api_thing_close( self.handle )

    def print( self ):
        self.lib.api_print_thing( self.handle )

    def command( self, cmd ):
        # convert cmd into c
        ccmd = cmd.encode()
        # set c integer for error value
        err = c_int()
        # call api
        err = self.lib.api_command( self.handle, ccmd )
        # error data back to py data
        ierr = int(err)
        # catching errors outside of the module
        if ierr == -1:
            msg = "Unknown command: '" + cmd +"'"
            raise ValueError( msg )
        return ierr

    def set( self, name, val ):
        # encode string into C
        cname = name.encode()

        # get datatype
        dtype = self.lib.api_get_datatype( self.handle, cname )

        # set proper C data to send
        if dtype == self._THING_DATA_INT:
            # set size of data (this is integer, so size 1)
            s=np.array([1, 1])

            # change dtype of size into c integers:
            #    Normally, python integers have a 0 behind them,
            #    this call removes the following zeros
            s=np.intc(s)
            # cast s as pointer to array
            csize = s.ctypes.data_as( POINTER(c_int) )

            # cast data to c type
            cdata = c_int(val)

            # argtype for cdata in call to api function
            cdata_typ = POINTER(POINTER(c_int))

            # repeat some version of above for other types
        if dtype == self._THING_DATA_INT_1D:
            s=np.array( [np.size(val), 1] )
            s=np.intc(s)
            csize = s.ctypes.data_as( POINTER(c_int) )
            val = np.intc(val)
            cdata = val.ctypes.data_as( POINTER(c_int) )
            cdata_typ = POINTER(POINTER(c_int))

        if dtype == self._THING_DATA_INT_2D:
            # transpose the array size for fortran
            s=np.array( [np.size(val,1), np.size(val,0)] )
            s=np.intc(s)
            csize = s.ctypes.data_as( POINTER(c_int) )
            val = np.intc(val)
            cdata = val.ctypes.data_as( POINTER(c_int) )
            cdata_typ = POINTER(POINTER(c_int))

        # all similar for real
        if dtype == self._THING_DATA_REAL:
            s=np.array([1, 1])
            s=np.intc(s)
            csize = s.ctypes.data_as( POINTER(c_int) )
            # api is in single precision -> use c_float
            cdata = c_float(val)
            cdata_typ = POINTER(POINTER(c_float))

        if dtype == self._THING_DATA_REAL_1D:
            s=np.array([np.size(val), 1])
            s=np.intc(s)
            csize = s.ctypes.data_as( POINTER(c_int) )
            # np.float32 is equivalent to c_float
            val = np.float32( val )
            cdata = val.ctypes.data_as( POINTER(c_float) )
            cdata_typ = POINTER(POINTER(c_float))

        if dtype == self._THING_DATA_REAL_2D:
            s=np.array([np.size(val,1), np.size(val,0)])
            s=np.intc(s)
            csize = s.ctypes.data_as( POINTER(c_int) )
            val = np.float32( val )
            cdata = val.ctypes.data_as( POINTER(c_float) )
            cdata_typ = POINTER(POINTER(c_float))

        if dtype == self._THING_DATA_STR:
            s=np.array([1, 1])
            s=np.intc(s)
            csize = s.ctypes.data_as( POINTER(c_int) )
            # this is passed differently than in command()!
            cdata=c_char_p(val.encode())
            cdata_typ = POINTER(c_char_p)

        # set argtypes
        self.lib.api_set_thing.argtypes = [ c_void_p, c_char_p, POINTER(POINTER(c_int)), cdata_typ ]

        # call api function
        # notice the handle and cname have the "value" attribute in api_set_thing()
        # and the csize and cdata do not have it.
        self.lib.api_set_thing( self.handle, cname, pointer(csize), pointer(cdata) )

        return



    def get( self, name ):
        # encode string into C
        cname = name.encode()

        # get datatype
        dtype = self.lib.api_get_datatype( self.handle, cname )

        # if dtype == self._THING_DATA_UNKNOWN:

        if dtype == self._THING_DATA_INT:
            c_data = c_int
            # set result type of function
            self.lib.api_get_thing.restype = POINTER(c_int)
            # call it
            c_data = self.lib.api_get_thing( self.handle, cname )
            # convert to python
            data = np.int( c_data[0] )
            return data

        if dtype == self._THING_DATA_REAL:
            c_data = c_float
            # set result type of function
            self.lib.api_get_thing.restype = POINTER(c_float)
            # call it
            c_data = self.lib.api_get_thing( self.handle, cname )
            # convert to python
            data = np.float( c_data[0] )
            return data

        if dtype == self._THING_DATA_INT_1D:
            # get size
            csize = c_int(2)
            csize = self.lib.api_get_datasize( self.handle, cname )
            n = int( csize[0] )
            m = int( csize[1] )
            # allocate c-data array of size n
            c_data = (c_int*n)()
            self.lib.api_get_thing.restype = POINTER(c_int)
            # get data
            c_data = self.lib.api_get_thing( self.handle, cname )
            # convert to python
            data = np.ndarray(n, dtype=int )
            for i in range(n):
                data[i] = np.int( c_data[i] )
            return data

        if dtype == self._THING_DATA_INT_2D:
            # get size
            csize = c_int(2)
            csize = self.lib.api_get_datasize( self.handle, cname )
            n = int( csize[0] )
            m = int( csize[1] )
            # allocate c-data array of size n*m
            c_data = (c_int*n*m)()
            self.lib.api_get_thing.restype = POINTER(c_int)
            c_data = self.lib.api_get_thing( self.handle, cname )
            data = np.ndarray((m,n), dtype=int )
            k=0
            for i in range(m):
                for j in range(n):
                    data[i][j] = np.int( c_data[k] )
                    k+=1
            return data

        if dtype == self._THING_DATA_REAL_1D:
            # get size
            csize = c_int(2)
            csize = self.lib.api_get_datasize( self.handle, cname )
            n = int( csize[0] )
            m = int( csize[1] )
            # allocate c-data array
            c_data = (c_float*n)()
            self.lib.api_get_thing.restype = POINTER(c_float)
            # get data
            c_data = self.lib.api_get_thing( self.handle, cname )
            data = np.ndarray(n, dtype=float )
            for i in range(n):
                data[i] = np.float( c_data[i] )
            return data

        if dtype == self._THING_DATA_REAL_2D:
            # get size
            csize = c_int(2)
            csize = self.lib.api_get_datasize( self.handle, cname )
            n = int( csize[0] )
            m = int( csize[1] )
            # allocate c-data array
            c_data = (c_float*n*m)()
            self.lib.api_get_thing.restype = POINTER(c_float)
            c_data = self.lib.api_get_thing( self.handle, cname )
            data = np.ndarray((m,n), dtype=float )
            k=0
            for i in range(m):
                for j in range(n):
                    data[i][j] = np.float( c_data[k] )
                    k+=1
            return data

        if dtype == self._THING_DATA_STR:
            self.lib.api_get_thing.restype = c_char_p
            c_data = self.lib.api_get_thing( self.handle, cname )
            return c_data.decode()

