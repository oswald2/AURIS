

#include <stdio.h>

static unsigned char m_sregVal[128][256];

unsigned char lookup(int sreg, int xval)
{
    return m_sregVal[sreg][xval];
}


unsigned char codProcChar(int xval, int sreg)
{
    for(int mask = 0x80; mask != 0; mask >>= 1)
    {
        sreg <<= 1;                      // links schieben
        int bit = (sreg & 0x80) ? 1 : 0; // Herausfallendes Bit
        if(xval & mask)                  // mit Datenbit addieren
        {
            bit ^= 1;
        }

        if(bit)
        {
            sreg ^= 0x45;                  // Bit bei 0, 2 und 6 adieren
        }
    }
    sreg &= 0x7f;
    return (unsigned char)sreg;
}

void initialise()
{
    printf("C: initialize called...\n");

    static int initialized = 0;

    if(0 == initialized)
    {
        initialized = 1;
        printf("C: initializing...\n");

        // Possible contents of the shift register
        for(int sreg = 0; sreg < 128; sreg++)
        {
            // Loop over all possible values of the shift register
            for(int value = 0; value < 256; value++)
            {
                // Loop over all possible values of the input byte
                // Calculate the new shift register value in dependency
                // of the previous value and the input byte
                // Insert this new value in the array
                m_sregVal[sreg][value] = codProcChar(value, sreg);
            }
        }
        printf("C: initialized.\n");
    }
}

unsigned char check(const unsigned char* const dataPtr,
        const short dataLength)
{
    int sreg = 0;  // initialize the shift register with 0

    // Loop over all octets in the information field
    for(int octetNo = 0; octetNo < dataLength; octetNo++)
    {
        // Fetch the information octet
        int val = dataPtr[octetNo];

        // Fetch the new shift register value from the big array
        sreg = m_sregVal[sreg][val];
    }

    // The sreg value remaining has to be inverted, is passed as the 7 most
    // significatnt bits of the octet and a 0 is appended as LSB of the CRC
    sreg ^= 0xFF;  // invert
    sreg <<= 1;    // make it the 7 most sign. bits

    return (unsigned char)(sreg & 0xFE);  // append a 0
}
// =========================================================================
