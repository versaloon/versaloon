// Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com> 

#ifndef __EMIIC_MOD_INCLUDED__
#define __EMIIC_MOD_INCLUDED__

#define DECLARE_EMIIC_MOD(MOD_NAME, DLY_TYPE)      \
    extern IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_SetParameter(DLY_TYPE ClockCycle, DLY_TYPE D_MaxDly, DLY_TYPE D_DlyStep, DLY_TYPE ByteInterval);\
    extern IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_Init(void);\
    extern IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_DeInit(void);\
    extern IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_Send(uint8_t addr, uint8_t *buff, uint16_t len, IIC_STOP_t stop, bool nacklast, uint16_t *actual_len);\
    extern IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_Receive(uint8_t addr, uint8_t *buff, uint16_t len, IIC_STOP_t stop, uint16_t *actual_len);

// MOD_NAME:name of IIC module
// SCL_D:   function to set SCL dominant(output 0), void SCL_D(void);
// SCL_R:   function to set SCL recessive(input),   void SCL_R(void);
// SCL_G:   function to get SCL state,              uint8_t SCL_G(void);
// SDA_D:   function to set SDA dominant(output 0)  void SDA_D(void);
// SDA_R:   function to set SDA recessive(input)    void SDA_R(void);
// SDA_G:   function to get SDA state,              uint8_t SDA_G(void);
// DLY_FUNC:function to generate delay,             void DLY_FUNC(DLY_TYPE dly);
// DLY_TYPE:type of delay variable
#define DEFINE_EMIIC_MOD(MOD_NAME, SCL_D, SCL_R, SCL_G, SDA_D, SDA_R, SDA_G, DLY_FUNC, DLY_TYPE)    \
    static DLY_TYPE s_EMIIC_##MOD_NAME##_QuarterCycle_Len = 0;\
    static DLY_TYPE s_EMIIC_##MOD_NAME##_D_MaxDly = 0;\
    static DLY_TYPE s_EMIIC_##MOD_NAME##_D_DlyStep = 0;\
    static DLY_TYPE s_EMIIC_##MOD_NAME##_ByteInterval = 0;\
    \
    IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_SetParameter(DLY_TYPE ClockCycle, DLY_TYPE D_MaxDly, DLY_TYPE D_DlyStep, DLY_TYPE ByteInterval)\
    {\
        s_EMIIC_##MOD_NAME##_QuarterCycle_Len = ClockCycle / 4;\
        if (!s_EMIIC_##MOD_NAME##_QuarterCycle_Len && ClockCycle)\
        {\
            s_EMIIC_##MOD_NAME##_QuarterCycle_Len = 1;\
        }\
        if (D_MaxDly)\
        {\
            s_EMIIC_##MOD_NAME##_D_MaxDly = D_MaxDly;\
        }\
        else\
        {\
            s_EMIIC_##MOD_NAME##_D_MaxDly = 1;\
        }\
        s_EMIIC_##MOD_NAME##_D_DlyStep = D_DlyStep;\
        s_EMIIC_##MOD_NAME##_ByteInterval = ByteInterval;\
        return IIC_MOD_ACK;\
    }\
    \
    static IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_WaitSCL_R(void)\
    {\
        DLY_TYPE dly = 0;\
        \
        while(!SCL_G())\
        {\
            DLY_FUNC(s_EMIIC_##MOD_NAME##_D_DlyStep);\
            dly += s_EMIIC_##MOD_NAME##_D_DlyStep;\
            if ((s_EMIIC_##MOD_NAME##_D_MaxDly > 0) \
                && (dly >= s_EMIIC_##MOD_NAME##_D_MaxDly))\
            {\
                return IIC_MOD_TO;\
            }\
        }\
        return IIC_MOD_ACK;\
    }\
    static IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_WaitSDA_R(void)\
    {\
        DLY_TYPE dly = 0;\
        \
        while(!SDA_G())\
        {\
            DLY_FUNC(s_EMIIC_##MOD_NAME##_D_DlyStep);\
            dly += s_EMIIC_##MOD_NAME##_D_DlyStep;\
            if ((s_EMIIC_##MOD_NAME##_D_MaxDly > 0) \
                && (dly >= s_EMIIC_##MOD_NAME##_D_MaxDly))\
            {\
                return IIC_MOD_TO;\
            }\
        }\
        return IIC_MOD_ACK;\
    }\
    \
    static IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_Start(void)\
    {\
        uint8_t retry = 0;\
        \
        SDA_R();\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len);\
        SCL_R();\
        if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSCL_R())\
        {\
            return IIC_MOD_TO;\
        }\
		if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSDA_R())\
		{\
        	while (!SDA_G())\
        	{\
        	    DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
        	    SCL_D();\
        	    DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
        	    SCL_R();\
        	    if ((IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSCL_R()) \
        	        || (retry++ > 9))\
        	    {\
        	        return IIC_MOD_TO;\
        	    }\
        	}\
		}\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
        SDA_D();\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
        SCL_D();\
        return IIC_MOD_ACK;\
    }\
    \
    static IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_Stop(void)\
    {\
        SCL_D();\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len);\
        SDA_D();\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len);\
        SCL_R();\
        if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSCL_R())\
        {\
            return IIC_MOD_TO;\
        }\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
        SDA_R();\
        if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSDA_R())\
        {\
            return IIC_MOD_TO;\
        }\
        return IIC_MOD_ACK;\
    }\
    \
    static IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_SendByte(uint8_t byte)\
    {\
        uint8_t i;\
        \
        for (i = 0; i < 8; i++)\
        {\
            DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len);\
            if (byte & 0x80)\
            {\
                SDA_R();\
                if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSDA_R())\
                {\
                    return IIC_MOD_TO;\
                }\
            }\
            else\
            {\
                SDA_D();\
            }\
            byte <<= 1;\
            \
            DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len);\
            SCL_R();\
            if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSCL_R())\
            {\
                return IIC_MOD_TO;\
            }\
            DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
            SCL_D();\
        }\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len);\
        SDA_R();\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len);\
        SCL_R();\
        if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSCL_R())\
        {\
            return IIC_MOD_TO;\
        }\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
        if (SDA_G())\
        {\
            SCL_D();\
            DLY_FUNC(s_EMIIC_##MOD_NAME##_ByteInterval);\
            return IIC_MOD_NACK;\
        }\
        else\
        {\
            SCL_D();\
            DLY_FUNC(s_EMIIC_##MOD_NAME##_ByteInterval);\
            return IIC_MOD_ACK;\
        }\
    }\
    \
    static IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_ReceiveByte(uint8_t *byte, bool last)\
    {\
        uint8_t i;\
        \
        *byte = 0;\
        SDA_R();\
        for (i = 0; i < 8; i++)\
        {\
            DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
            SCL_R();\
            if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSCL_R())\
            {\
                return IIC_MOD_TO;\
            }\
            DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
            (*byte) <<= 1;\
            if (SDA_G())\
            {\
                *byte |= 1;\
            }\
            SCL_D();\
        }\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len);\
        if (last)\
        {\
            SDA_R();\
            if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSDA_R())\
            {\
                return IIC_MOD_TO;\
            }\
        }\
        else\
        {\
            SDA_D();\
        }\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len);\
        SCL_R();\
        if (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSCL_R())\
        {\
            return IIC_MOD_TO;\
        }\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_QuarterCycle_Len * 2);\
        SCL_D();\
        DLY_FUNC(s_EMIIC_##MOD_NAME##_ByteInterval);\
        return IIC_MOD_ACK;\
    }\
    \
    IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_Send(uint8_t addr, uint8_t *buff, uint16_t len, IIC_STOP_t stop, uint16_t *actual_len)\
    {\
        IIC_MOD_RESULT_t result;\
        \
        *actual_len = 0;\
        addr &= ~1;\
        \
        if ((IIC_MOD_ACK != EMIIC_##MOD_NAME##_Start()) \
            || (IIC_MOD_ACK != EMIIC_##MOD_NAME##_SendByte(addr)))\
        {\
            EMIIC_##MOD_NAME##_Stop();\
            return IIC_MOD_ADDR_NO_RESPONSE;\
        }\
        \
        for (; *actual_len < len; (*actual_len)++)\
        {\
            result = EMIIC_##MOD_NAME##_SendByte(buff[*actual_len]);\
            if (IIC_MOD_TO == result)\
            {\
                EMIIC_##MOD_NAME##_Stop();\
                return IIC_MOD_TO;\
            }\
            else if (IIC_MOD_NACK == result)\
            {\
                (*actual_len)++;\
                break;\
            }\
        }\
        \
        if (IIC_FORCESTOP == stop)\
        {\
            EMIIC_##MOD_NAME##_Stop();\
        }\
        \
        if (*actual_len < len)\
        {\
            return IIC_MOD_NACK;\
        }\
        else\
        {\
            return IIC_MOD_ACK;\
        }\
    }\
    \
    IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_Receive(uint8_t addr, uint8_t *buff, uint16_t len, IIC_STOP_t stop, bool nacklast, uint16_t *actual_len)\
    {\
        IIC_MOD_RESULT_t result;\
        \
        *actual_len = 0;\
        addr |= 1;\
        \
        if ((IIC_MOD_ACK != EMIIC_##MOD_NAME##_Start()) \
            || (IIC_MOD_ACK != EMIIC_##MOD_NAME##_SendByte(addr)))\
        {\
            EMIIC_##MOD_NAME##_Stop();\
            return IIC_MOD_ADDR_NO_RESPONSE;\
        }\
        \
        for (; *actual_len < len; (*actual_len)++)\
        {\
            result = EMIIC_##MOD_NAME##_ReceiveByte(&buff[*actual_len], nacklast && (*actual_len == (len - 1)));\
            if (IIC_MOD_TO == result)\
            {\
                EMIIC_##MOD_NAME##_Stop();\
                return IIC_MOD_TO;\
            }\
            else if (IIC_MOD_NACK == result)\
            {\
                (*actual_len)++;\
                break;\
            }\
        }\
        \
        if (IIC_FORCESTOP == stop)\
        {\
            EMIIC_##MOD_NAME##_Stop();\
        }\
        \
        if (*actual_len < len)\
        {\
            return IIC_MOD_NACK;\
        }\
        else\
        {\
            return IIC_MOD_ACK;\
        }\
    }\
    IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_Init(void)\
    {\
        SCL_R();\
        SDA_R();\
        if ((IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSCL_R()) \
            || (IIC_MOD_ACK != EMIIC_##MOD_NAME##_WaitSDA_R()))\
        {\
            return IIC_MOD_NACK;\
        }\
        return IIC_MOD_ACK;\
    }\
    \
    IIC_MOD_RESULT_t EMIIC_##MOD_NAME##_DeInit(void)\
    {\
        return EMIIC_##MOD_NAME##_Init();\
    }

#endif  // __EMIIC_MOD_INCLUDED__
