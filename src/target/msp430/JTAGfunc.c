// This file is rewritten from the same file in slaa149f
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "app_cfg.h"
#if TARGET_MSP430_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "msp430.h"
#include "msp430_internal.h"
#include "JTAGfunc.h"

static uint8_t ir;
static uint32_t dr;

//----------------------------------------------------------------------------
/* Function to set target CPU JTAG FSM into the instruction fetch state
   Argument: None
   Result:   word (STATUS_OK if instr. fetch was set, STATUS_ERROR otherwise)
*/
void SetInstrFetch(void)
{
    IR_Shift(IR_CNTRL_SIG_CAPTURE);

    // Wait until CPU is in instr. fetch state, timeout after limited attempts
	msp430_jtag_dr16_poll(0x0000, 0x0080, 0x0080, 50, 1);
}

//----------------------------------------------------------------------------
/* Load a given address into the target CPU's program counter (PC).
   Argument: word Addr (destination address)
   Result:   None
*/
void SetPC(word Addr)
{
    SetInstrFetch();              // Set CPU into instruction fetch mode, TCLK=1

    // Load PC with address
    IR_Shift(IR_CNTRL_SIG_16BIT);
    DR_Shift16(0x3401);           // CPU has control of RW & BYTE.
    IR_Shift(IR_DATA_16BIT);
    DR_Shift16(0x4030);           // "mov #addr,PC" instruction
    ClrTCLK();
    SetTCLK();                    // F2xxx
    DR_Shift16(Addr);             // "mov #addr,PC" instruction
    ClrTCLK();
    IR_Shift(IR_ADDR_CAPTURE);
    SetTCLK();
    ClrTCLK();                    // Now the PC should be on Addr
    IR_Shift(IR_CNTRL_SIG_16BIT);
    DR_Shift16(0x2401);           // JTAG has control of RW & BYTE.
}

//----------------------------------------------------------------------------
/* Function to release the target device from JTAG control
   Argument: word Addr (0xFFFE: Perform Reset, means Load Reset Vector into PC,
                        otherwise: Load Addr into PC)
   Result:   None
*/
void ReleaseDevice(word Addr)
{
    if (Addr == V_RESET)
    {
        IR_Shift(IR_CNTRL_SIG_16BIT);
        DR_Shift16(0x2C01);         // Perform a reset
        DR_Shift16(0x2401);
    }
    else
    {
        SetPC(Addr);                // Set target CPU's PC
    }
    IR_Shift(IR_CNTRL_SIG_RELEASE);
}

//----------------------------------------------------------------------------
/* Function to set the CPU into a controlled stop state
*/
void HaltCPU(void)
{
    SetInstrFetch();              // Set CPU into instruction fetch mode

    IR_Shift(IR_DATA_16BIT);
    DR_Shift16(0x3FFF);           // Send JMP $ instruction
    ClrTCLK();
    IR_Shift(IR_CNTRL_SIG_16BIT);
    DR_Shift16(0x2409);           // Set JTAG_HALT bit
    SetTCLK();
}

//----------------------------------------------------------------------------
/* Function to release the target CPU from the controlled stop state
*/
void ReleaseCPU(void)
{
    ClrTCLK();
    IR_Shift(IR_CNTRL_SIG_16BIT);
    DR_Shift16(0x2401);           // Clear the HALT_JTAG bit
    IR_Shift(IR_ADDR_CAPTURE);
    SetTCLK();
}

//----------------------------------------------------------------------------
/* This function reads one byte/word from a given address in memory
   Arguments: word Format (F_BYTE or F_WORD)
              word Addr (address of memory)
   Result:    word (content of the addressed memory location)
*/
void ReadMem(word Format, word Addr, word *ptr)
{
    HaltCPU();

    ClrTCLK();
    IR_Shift(IR_CNTRL_SIG_16BIT);
    if  (Format == F_WORD)
    {
        DR_Shift16(0x2409);         // Set word read
    }
    else
    {
        DR_Shift16(0x2419);         // Set byte read
    }
    IR_Shift(IR_ADDR_16BIT);
    DR_Shift16(Addr);               // Set address
    IR_Shift(IR_DATA_TO_ADDR);
    SetTCLK();

    ClrTCLK();
    DR_Shift16_Read(0x0000, ptr);   // Shift out 16 bits

    ReleaseCPU();
}

//----------------------------------------------------------------------------
/* This function writes one byte/word at a given address ( <0xA00)
   Arguments: word Format (F_BYTE or F_WORD)
              word Addr (Address of data to be written)
              word Data (shifted data)
   Result:    None
*/
void WriteMem(word Format, word Addr, word Data)
{
    HaltCPU();

    ClrTCLK();
    IR_Shift(IR_CNTRL_SIG_16BIT);
    if  (Format == F_WORD)
    {
        DR_Shift16(0x2408);     // Set word write
    }
    else
    {
        DR_Shift16(0x2418);     // Set byte write
    }
    IR_Shift(IR_ADDR_16BIT);
    DR_Shift16(Addr);           // Set addr
    IR_Shift(IR_DATA_TO_ADDR);
    DR_Shift16(Data);           // Shift in 16 bits
    SetTCLK();

    ReleaseCPU();
}

//----------------------------------------------------------------------------
/* This function reads an array of words from a memory.
   Arguments: word StartAddr (Start address of memory to be read)
              word Length (Number of words to be read)
              word *DataArray (Pointer to array for the data)
   Result:    None
*/
void ReadMemQuick(word StartAddr, word Length, word *DataArray)
{
    word i;

    // Initialize reading:
    SetPC(StartAddr-4);
    HaltCPU();

    ClrTCLK();
    IR_Shift(IR_CNTRL_SIG_16BIT);
    DR_Shift16(0x2409);                    // Set RW to read
    IR_Shift(IR_DATA_QUICK);

    for (i = 0; i < Length; i++)
    {
        SetTCLK();
        DR_Shift16_Read(0x0000, DataArray + i); // Shift out the data
                                           // from the target.
        ClrTCLK();
    }
    ReleaseCPU();
}

//----------------------------------------------------------------------------
/* This function writes an array of words into the target memory.
   Arguments: word StartAddr (Start address of target memory)
              word Length (Number of words to be programmed)
              word *DataArray (Pointer to array with the data)
   Result:    None
*/
void WriteMemQuick(word StartAddr, word Length, word *DataArray)
{
    word i;

    // Initialize writing:
    SetPC((word)(StartAddr-4));
    HaltCPU();

    ClrTCLK();
    IR_Shift(IR_CNTRL_SIG_16BIT);
    DR_Shift16(0x2408);             // Set RW to write
    IR_Shift(IR_DATA_QUICK);
    for (i = 0; i < Length; i++)
    {
        DR_Shift16(DataArray[i]);   // Shift in the write data
        SetTCLK();
        ClrTCLK();                  // Increment PC by 2
    }
    ReleaseCPU();
}

//----------------------------------------------------------------------------
/* This function programs/verifies an array of words into an FLASH by
   using the FLASH controller.
   Arguments: word StartAddr (Start address of FLASH memory)
              word Length (Number of words to be programmed)
              word *DataArray (Pointer to array with the data)
   Result:    None
*/
void WriteFLASH(word StartAddr, word Length, word *DataArray)
{
    word i;                     // Loop counter
    word addr = StartAddr;      // Address counter
    word FCTL3_val = 0xA500;    // ok for all devices; if Info-Seg. A on F2xxx should not be programmed
//  word FCTL3_val = 0xA540;    // only if Info-Seg. A on F2xxx should be programmed

    HaltCPU();

    ClrTCLK();
    IR_Shift(IR_CNTRL_SIG_16BIT);
    DR_Shift16(0x2408);         // Set RW to write
    IR_Shift(IR_ADDR_16BIT);
    DR_Shift16(0x0128);         // FCTL1 register
    IR_Shift(IR_DATA_TO_ADDR);
    DR_Shift16(0xA540);         // Enable FLASH write
    SetTCLK();

    ClrTCLK();
    IR_Shift(IR_ADDR_16BIT);
    DR_Shift16(0x012A);         // FCTL2 register
    IR_Shift(IR_DATA_TO_ADDR);
    DR_Shift16(0xA540);         // Select MCLK as source, DIV=1
    SetTCLK();

    ClrTCLK();
    IR_Shift(IR_ADDR_16BIT);
    DR_Shift16(0x012C);         // FCTL3 register
    IR_Shift(IR_DATA_TO_ADDR);
    DR_Shift16(FCTL3_val);      // Clear FCTL3; F2xxx: Unlock Info-Seg.
                                // A by toggling LOCKA-Bit if required,
    SetTCLK();

    ClrTCLK();
    IR_Shift(IR_CNTRL_SIG_16BIT);

    for (i = 0; i < Length; i++, addr += 2)
    {
        DR_Shift16(0x2408);             // Set RW to write
        IR_Shift(IR_ADDR_16BIT);
        DR_Shift16(addr);               // Set address
        IR_Shift(IR_DATA_TO_ADDR);
        DR_Shift16(DataArray[i]);       // Set data
        SetTCLK();
        ClrTCLK();
        IR_Shift(IR_CNTRL_SIG_16BIT);
        DR_Shift16(0x2409);             // Set RW to read

        TCLKstrobes(35);        // Provide TCLKs, min. 33 for F149 and F449
                                // F2xxx: 29 are ok
    }

    IR_Shift(IR_CNTRL_SIG_16BIT);
    DR_Shift16(0x2408);         // Set RW to write
    IR_Shift(IR_ADDR_16BIT);
    DR_Shift16(0x0128);         // FCTL1 register
    IR_Shift(IR_DATA_TO_ADDR);
    DR_Shift16(0xA500);         // Disable FLASH write
    SetTCLK();

    // set LOCK-Bits again
    ClrTCLK();
    IR_Shift(IR_ADDR_16BIT);
    DR_Shift16(0x012C);         // FCTL3 address
    IR_Shift(IR_DATA_TO_ADDR);
    DR_Shift16(FCTL3_val);      // Lock Inf-Seg. A by toggling LOCKA and set LOCK again
    SetTCLK();

    ReleaseCPU();
}

//----------------------------------------------------------------------------
/* This function performs a mass erase (with and w/o info memory) or a segment
   erase of a FLASH module specified by the given mode and address.
   Large memory devices get additional mass erase operations to meet the spec.
   Arguments: word Mode (could be ERASE_MASS or ERASE_MAIN or ERASE_SGMT)
              word Addr (any address within the selected segment)
   Result:    None
   Remark:    Could be extended with erase check via PSA.
*/
void EraseFLASH(struct program_context_t *context, word EraseMode, word EraseAddr)
{
    word StrobeAmount = 4820;       // default for Segment Erase
    word i, loopcount = 1;          // erase cycle repeating for Mass Erase
    word FCTL3_val = 0xA500;        // ok for all devices; if Info-Seg. A on F2xxx should not be erased
//  word FCTL3_val = 0xA540;        // only if Info-Seg. A on F2xxx should be erased

    if ((EraseMode == ERASE_MASS) || (EraseMode == ERASE_MAIN))
    {
        if(DeviceHas_FastFlash())
        {
            StrobeAmount = 10600;        // Larger Flash memories require
        }
        else
        {
            StrobeAmount = 5300;        // Larger Flash memories require
            loopcount = 19;             // additional cycles for erase.
        }
    }
    HaltCPU();

    for (i = loopcount; i > 0; i--)
    {
        ClrTCLK();
        IR_Shift(IR_CNTRL_SIG_16BIT);
        DR_Shift16(0x2408);         // set RW to write
        IR_Shift(IR_ADDR_16BIT);
        DR_Shift16(0x0128);         // FCTL1 address
        IR_Shift(IR_DATA_TO_ADDR);
        DR_Shift16(EraseMode);      // Enable erase mode
        SetTCLK();

        ClrTCLK();
        IR_Shift(IR_ADDR_16BIT);
        DR_Shift16(0x012A);         // FCTL2 address
        IR_Shift(IR_DATA_TO_ADDR);
        DR_Shift16(0xA540);         // MCLK is source, DIV=1
        SetTCLK();

        ClrTCLK();
        IR_Shift(IR_ADDR_16BIT);
        DR_Shift16(0x012C);         // FCTL3 address
        IR_Shift(IR_DATA_TO_ADDR);
        DR_Shift16(FCTL3_val);      // Clear FCTL3; F2xxx: Unlock Info-Seg. A by toggling LOCKA-Bit if required,
        SetTCLK();

        ClrTCLK();
        IR_Shift(IR_ADDR_16BIT);
        DR_Shift16(EraseAddr);      // Set erase address
        IR_Shift(IR_DATA_TO_ADDR);
        DR_Shift16(0x55AA);         // Dummy write to start erase
        SetTCLK();

        ClrTCLK();
        IR_Shift(IR_CNTRL_SIG_16BIT);
        DR_Shift16(0x2409);         // Set RW to read
        TCLKstrobes(StrobeAmount);  // Provide TCLKs
        IR_Shift(IR_CNTRL_SIG_16BIT);
        DR_Shift16(0x2408);         // Set RW to write
        IR_Shift(IR_ADDR_16BIT);
        DR_Shift16(0x0128);         // FCTL1 address
        IR_Shift(IR_DATA_TO_ADDR);
        DR_Shift16(0xA500);         // Disable erase
        SetTCLK();
    }
    // set LOCK-Bits again
    ClrTCLK();
    IR_Shift(IR_ADDR_16BIT);
    DR_Shift16(0x012C);         // FCTL3 address
    IR_Shift(IR_DATA_TO_ADDR);
    DR_Shift16(FCTL3_val);      // Lock Inf-Seg. A by toggling LOCKA (F2xxx) and set LOCK again
    SetTCLK();

    ReleaseCPU();
}

//----------------------------------------------------------------------------
/* Function to execute a Power-On Reset (POR) using JTAG CNTRL SIG register
   Arguments: None
   Result:    word (STATUS_OK if JTAG ID is valid, STATUS_ERROR otherwise)
*/
void ExecutePOR(void)
{
    // Perform Reset
    IR_Shift(IR_CNTRL_SIG_16BIT);
    DR_Shift16(0x2C01);                 // Apply Reset
    DR_Shift16(0x2401);                 // Remove Reset
    ClrTCLK();
    SetTCLK();
    ClrTCLK();
    SetTCLK();
    ClrTCLK();
    IR_Shift(IR_ADDR_CAPTURE); // read JTAG ID, checked at function end
    SetTCLK();

    WriteMem(F_WORD, 0x0120, 0x5A80);   // Disable Watchdog on target device
}

//----------------------------------------------------------------------------
/* This function compares the computed PSA (Pseudo Signature Analysis) value
   to the PSA value shifted out from the target device.
   It is used for very fast data block write or erasure verification.
   Arguments: word StartAddr (Start address of data block to be checked)
              word Length (Number of words within data block)
              word *DataArray (Pointer to array with the data, 0 for Erase Check)
   Result:    word (STATUS_OK if comparison was successful, STATUS_ERROR otherwise)
*/
static word VerifyPSA(struct program_context_t *context, word StartAddr, word Length, word *DataArray, word *CRC)
{
    word i;
    word POLY = 0x0805;           // Polynom value for PSA calculation
    word PSA_CRC = StartAddr-2;   // Start value for PSA calculation

    ExecutePOR();

    if(DeviceHas_EnhVerify())
    {
        SetPC(StartAddr-4);
        HaltCPU();
        ClrTCLK();
        IR_Shift(IR_DATA_16BIT);
        DR_Shift16(StartAddr-2);
    }
    else
    {
        SetPC(StartAddr-2);
        SetTCLK();
        ClrTCLK();
    }
    IR_Shift(IR_DATA_PSA);
    for (i = 0; i < Length; i++)
    {
        // Calculate the PSA (Pseudo Signature Analysis) value
        if ((PSA_CRC & 0x8000) == 0x8000)
        {
            PSA_CRC ^= POLY;
            PSA_CRC <<= 1;
            PSA_CRC |= 0x0001;
        }
        else
        {
            PSA_CRC <<= 1;
        }
        // if pointer is 0 then use erase check mask, otherwise data
        &DataArray[0] == 0 ? (PSA_CRC ^= 0xFFFF) : (PSA_CRC ^= DataArray[i]);

        // Clock through the PSA
        SetTCLK();
//        ClrTCLK();           // set here -> Fixes problem with F123 PSA in RAM

		DR_Shift0();

        ClrTCLK();           // set here -> future purpose
    }
    IR_Shift(IR_SHIFT_OUT_PSA);
    DR_Shift16_Read(0x0000, CRC);     // Read out the PSA value
    SetTCLK();

    if(DeviceHas_EnhVerify())
    {
        ReleaseCPU();
    }
    ExecutePOR();

	return PSA_CRC;
}

//----------------------------------------------------------------------------
/* This function performs an Erase Check over the given memory range
   Arguments: word StartAddr (Start address of memory to be checked)
              word Length (Number of words to be checked)
   Result:    word (STATUS_OK if erase check was successful, STATUS_ERROR otherwise)
*/
word EraseCheck(struct program_context_t *context, word StartAddr, word Length, word *CRC)
{
    return (VerifyPSA(context, StartAddr, Length, 0, CRC));
}

//----------------------------------------------------------------------------
/* This function performs a Verification over the given memory range
   Arguments: word StartAddr (Start address of memory to be verified)
              word Length (Number of words to be verified)
              word *DataArray (Pointer to array with the data)
   Result:    word (STATUS_OK if verification was successful, STATUS_ERROR otherwise)
*/
word VerifyMem(struct program_context_t *context, word StartAddr, word Length, word *DataArray, word *CRC)
{
    return (VerifyPSA(context, StartAddr, Length, DataArray, CRC));
}

//------------------------------------------------------------------------
/* This function blows the security fuse.
   Arguments: None
   Result:    word (TRUE if burn was successful, FALSE otherwise)
*/
word BlowFuse(void)
{
/*
    word mode = VPP_ON_TEST;        // Devices with TEST pin: VPP to TEST

    if(!DeviceHas_TestPin())
    {
        // Devices without TEST pin
        IR_Shift(IR_CNTRL_SIG_16BIT);// TDO becomes TDI functionality
        DR_Shift16(0x7201);
        TDOisInput();
        mode = VPP_ON_TDI;          // Enable VPP on TDI
    }

    IR_Shift(IR_PREPARE_BLOW);      // Initialize fuse blowing
    MsDelay(1);
    VPPon(mode);                    // Switch VPP onto selected pin
    MsDelay(5);
    IR_Shift(IR_EX_BLOW);           // Execute fuse blowing
    MsDelay(1);

    // Switch off power to target and wait
    ReleaseTarget();                // switch VPP and VCC target off
    MsDelay(200);

    // Check fuse: switch power on, simulate an initial JTAG entry
    InitTarget();                   // Supply and preset Target Board

    // Return result of "is fuse blown?"
    return(GetDevice() == STATUS_FUSEBLOWN);
*/
	return 0;
}

#endif
