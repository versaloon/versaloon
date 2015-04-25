#########################################################################################################################
#
# Here we define various hardware specific things about the board being built for
#
#########################################################################################################################

# Available boards:
# NanoRelease1
# MiniRelease1
# ProRelease1
# ProRelease3
# STBee_Mini
# STM8S_Discovery
# STM32VL_Discovery
# STM32L_Discovery
# STM32F4_Discovery

# Available targets:
# LowDensity
# LowDensityValueLine
# MediumDensity
# MediumDensityValueLine
# HighDensity
# ConnectivityLine
# XLDensity
#
# This is selected per board by TARGET_STM32 = <target>
# Because vsf library use XLDensity as default, MUST be set to XLDensity
# and the firmware will be compatible
#

########################################################################
ifeq ($(HW_BOARD),NanoRelease1)
########################################################################
_HARDWARE_VER		= 0x01
FLASH_LOAD_OFFSET	= 0x2000
HSE_VALUE			= 12000000
LD_FILE				= versaloonSTM32.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
USR_DEFS+=-DCORE_DEBUG=STM32_DBG_NONE
else
########################################################################
ifeq ($(HW_BOARD),MiniRelease1)
########################################################################
_HARDWARE_VER		= 0x15
FLASH_LOAD_OFFSET	= 0x8000
HSE_VALUE			= 12000000
LD_FILE				= versaloonSTM32.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
USR_DEFS+=-DCORE_DEBUG=STM32_DBG_NONE
else
########################################################################
ifeq ($(HW_BOARD),ProRelease1)
########################################################################
_HARDWARE_VER		= 0x21
FLASH_LOAD_OFFSET	= 0x8000
HSE_VALUE			= 12000000
LD_FILE				= versaloonProSTM32.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
USR_DEFS+=-DCORE_DEBUG=STM32_DBG_NONE
else
########################################################################
ifeq ($(HW_BOARD),ProRelease3)
########################################################################
_HARDWARE_VER		= 0x23
FLASH_LOAD_OFFSET	= 0x8000
HSE_VALUE			= 12000000
LD_FILE				= versaloonProSTM32.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
USR_DEFS+=-DCORE_DEBUG=STM32_DBG_NONE
else
########################################################################
ifeq ($(HW_BOARD),STBee_Mini)
########################################################################
_HARDWARE_VER		= 0x31
FLASH_LOAD_OFFSET	= 0x3000
HSE_VALUE			= 12000000
LD_FILE				= stbee.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
else
########################################################################
ifeq ($(HW_BOARD),STM8S_Discovery)
########################################################################
_HARDWARE_VER		= 0x32
FLASH_LOAD_OFFSET	= 0x0000
HSE_VALUE			= 8000000
LD_FILE				= st-discovery.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
else
########################################################################
ifeq ($(HW_BOARD),STM8L_Discovery)
########################################################################
_HARDWARE_VER		= 0x36
FLASH_LOAD_OFFSET	= 0x0000
HSE_VALUE			= 8000000
LD_FILE				= st-discovery.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
else
########################################################################
ifeq ($(HW_BOARD),STM8SVL_Discovery)
########################################################################
_HARDWARE_VER		= 0x37
FLASH_LOAD_OFFSET	= 0x0000
HSE_VALUE			= 8000000
LD_FILE				= st-discovery.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
else
########################################################################
ifeq ($(HW_BOARD),STM32VL_Discovery)
########################################################################
_HARDWARE_VER		= 0x33
FLASH_LOAD_OFFSET	= 0x0000
HSE_VALUE			= 8000000
LD_FILE				= st-discovery.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
else
########################################################################
ifeq ($(HW_BOARD),STM32L_Discovery)
########################################################################
_HARDWARE_VER		= 0x34
FLASH_LOAD_OFFSET	= 0x2000
HSE_VALUE			= 8000000
LD_FILE				= st-discovery.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
else
########################################################################
ifeq ($(HW_BOARD),STM32F4_Discovery)
########################################################################
_HARDWARE_VER		= 0x35
FLASH_LOAD_OFFSET	= 0x0000
HSE_VALUE			= 8000000
LD_FILE				= st-discovery.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
else
########################################################################
ifeq ($(HW_BOARD),ST_Link)
########################################################################
_HARDWARE_VER		= 0x38
FLASH_LOAD_OFFSET	= 0x0000
HSE_VALUE			= 8000000
LD_FILE				= st-discovery.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
else
########################################################################
ifeq ($(HW_BOARD),STM32F0_Discovery)
########################################################################
_HARDWARE_VER		= 0x39
FLASH_LOAD_OFFSET	= 0x0000
HSE_VALUE			= 8000000
LD_FILE				= st-discovery.ld
TARGET_CHIP			= stm32
TARGET_STM32		= XLDensity
else
########################################################################
# Unknown board error
########################################################################
$(error Missing or unknown HW_BOARD defined in makefile)
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif

#
USR_DEFS+= -DHSE_VALUE=$(HSE_VALUE)UL -DHW_BOARD=$(HW_BOARD) -D_HARDWARE_VER=$(_HARDWARE_VER)
USR_DEFS+= -DFLASH_LOAD_OFFSET=$(FLASH_LOAD_OFFSET)
USR_LIBS+= -L../../
USR_DEFS+= -D__TARGET_CHIP__=$(TARGET_CHIP)
