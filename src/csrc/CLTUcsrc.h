
#ifndef _CLTU_c_src_
#define _CLTU_c_src_


unsigned char codProcChar(int xval, int sreg);

void initialise();

unsigned char check(const unsigned char* const dataPtr,
        const short dataLength);

unsigned char lookup(int sreg, int xval);

#endif