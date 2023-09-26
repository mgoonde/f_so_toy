The idea is to have source code in fortran (possibly OOF), and interface it through .so, such that other codes can read, send and receive data from it.
The source has an API, which is compiled into .so, and then called from interfaces.


compile:

    make


