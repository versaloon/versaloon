#include "app_cfg.h"
#include "app_type.h"

#include "stlink.h"

#include "app_interfaces.h"
#include "adi_v5p1.h"
#include "cm_common.h"

// STLINK commands
#define STLINK_CMD_GET_VERSION					0xF1
#define STLINK_CMD_ADI							0xF2
#define STLINK_CMD_DFU							0xF3
#define STLINK_CMD_SWIM							0xF4
#define STLINK_CMD_GET_MODE						0xF5
#define STLINK_CMD_GET_VOLTAGE					0xF7

enum stlink_mode_t
{
	STLINK_MODE_DFU = 0x00,
	STLINK_MODE_IDLE = 0x01,
	STLINK_MODE_ADI = 0x02,
	STLINK_MODE_SWIM = 0x03,
	STLINK_MODE_BOOTLOADER = 0x04
};

// DFU commands
#define STLINK_DFU_SUBCMD_EXIT					0x07

// SWIM commands & replies
#define STLINK_SWIM_SUBCMD_ENTER				0x00
#define STLINK_SWIM_SUBCMD_EXIT					0x01

// ADI commands & replies
#define STLINK_ADI_SUBCMD_FORCEDEBUG			0x02
#define STLINK_ADI_SUBCMD_RESETSYS				0x03
#define STLINK_ADI_SUBCMD_RUN					0x09
#define STLINK_ADI_SUBCMD_STEP					0x0A
#define STLINK_ADI_SUBCMD_READ_MEM32			0x07
#define STLINK_ADI_SUBCMD_WRITE_MEM32			0x08
#define STLINK_ADI_SUBCMD_READ_MEM8				0x0C
#define STLINK_ADI_SUBCMD_WRITE_MEM8			0x0D
#define STLINK_ADI_SUBCMD_EXIT					0x21
#define STLINK_ADI_SUBCMD_GETCOREID				0x22
#define STLINK_ADI_SUBCMD_ENTER					0x30
#define STLINK_ADI_SUBCMD_READ_IDCODE			0x31
#define STLINK_ADI_SUBCMD_RESET					0x32
#define STLINK_ADI_SUBCMD_READ_COREREG			0x33
#define STLINK_ADI_SUBCMD_WRITE_COREREG			0x34
#define STLINK_ADI_SUBCMD_WRITE_MEMAPREG		0x35
#define STLINK_ADI_SUBCMD_READ_MEMAPREG			0x36
#define STLINK_ADI_SUBCMD_SET_BREAKPOINT		0x38
#define STLINK_ADI_SUBCMD_CLEAR_BREAKPOINT		0x39
#define STLINK_ADI_SUBCMD_READ_ALLCOREREGS		0x3A
#define STLINK_ADI_SUBCMD_GETRWSTATUS			0x3B
#define STLINK_ADI_SUBCMD_NRST					0x3C
#define STLINK_ADI_SUBCMD_STATUS				0x3E
#define STLINK_ADI_SUBCMD_UNKNOWN40				0x40
#define STLINK_ADI_SUBCMD_UNKNOWN41				0x41
#define STLINK_ADI_SUBCMD_TRACE_SIZE			0x42

#define STLINK_ADI_ENTER_SWD					0xA3
#define STLINK_ADI_ENTER_JTAG					0xA4

#define STLINK_ADI_REP_OK						0x80
#define STLINK_ADI_REP_FAIL						0x81
#define STLINK_ADI_REP_APWAIT					0x10
#define STLINK_ADI_REP_RWFAIL					0x12
#define STLINK_ADI_REP_DPWAIT					0x14

enum stlink_nrst_t
{
	STLINK_NRST_LOW = 0x00,
	STLINK_NRST_HIGH = 0x01,
	STLINK_NRST_PULSE = 0x02,
};

struct stlink_t
{
	struct stlink_version_t
	{
		uint8_t stlink;
		uint8_t adi;
		uint8_t swim;
		
		uint16_t vid;
		uint16_t pid;
	} version;
	enum stlink_mode_t mode;
	union
	{
		struct
		{
			uint16_t rw_status;
			uint32_t rw_addr;
			
			struct cm_common_info_t cm_info;
			enum adi_dp_target_core_t core;
		} adi;
		struct
		{
			uint8_t dummy;
		} swim;
	}port;
} stlink =
{
	{
		2,					// stlink
		15,					// adi
		4,					// swim
		
		0x0483,				// vid
		0x3748				// pid
	},						// version
	STLINK_MODE_IDLE		// mode
};

uint16_t stlink_getpkgsize(uint8_t *cmd, uint16_t len)
{
	uint32_t datalen = 0;
	
	switch (cmd[0])
	{
	case STLINK_CMD_GET_VERSION:
	case STLINK_CMD_GET_MODE:
	case STLINK_CMD_GET_VOLTAGE:
		break;
	case STLINK_CMD_ADI:
		switch (cmd[1])
		{
		case STLINK_ADI_SUBCMD_WRITE_MEM32:
		case STLINK_ADI_SUBCMD_WRITE_MEM8:
			datalen = GET_LE_U16(&cmd[6]);
			break;
		}
	case STLINK_CMD_DFU:
		// TODO: add DFU support
		break;
	case STLINK_CMD_SWIM:
		// TODO: add SWIM support
		break;
	}
	return 16 + datalen;
}

uint16_t stlink_process(uint8_t *cmd, uint16_t len)
{
	switch (cmd[0])
	{
	case STLINK_CMD_GET_VERSION:
	{
		uint16_t version =
			((uint16_t)(stlink.version.stlink & 0x0F) << 12) |
			((uint16_t)(stlink.version.adi & 0x3F) << 6) |
			((uint16_t)(stlink.version.swim & 0x3F) << 0);
		SET_BE_U16(&cmd[0], version);
		SET_LE_U16(&cmd[2], stlink.version.vid);
		SET_LE_U16(&cmd[4], stlink.version.pid);
		return 6;
	}
	case STLINK_CMD_GET_MODE:
	{
		cmd[0] = 0;
		cmd[1] = (uint8_t)stlink.mode;
		return 2;
	}
	case STLINK_CMD_GET_VOLTAGE:
	{
		uint16_t voltage;
		interfaces->target_voltage.get(0, &voltage);
		
		SET_LE_U32(&cmd[0], (voltage / 2) * 4096 / 3300);
		SET_LE_U32(&cmd[4], 1200 * 4096 / 3300);
		return 8;
	}
	case STLINK_CMD_ADI:
		switch (cmd[1])
		{
		case STLINK_ADI_SUBCMD_READ_MEM32:
		{
			uint32_t addr = GET_LE_U32(&cmd[2]);
			uint16_t len = GET_LE_U16(&cmd[6]);
			
			if (adi_memap_read_buf32(addr, &cmd[0], len))
			{
				stlink.port.adi.rw_addr = addr;
				stlink.port.adi.rw_status = STLINK_ADI_REP_FAIL;
			}
			else
			{
				stlink.port.adi.rw_addr = 0;
				stlink.port.adi.rw_status = STLINK_ADI_REP_OK;
			}
			return len;
		}
		case STLINK_ADI_SUBCMD_WRITE_MEM32:
		{
			uint32_t addr = GET_LE_U32(&cmd[2]);
			uint16_t len = GET_LE_U16(&cmd[6]);
			
			if (adi_memap_write_buf32(addr, &cmd[16], len))
			{
				stlink.port.adi.rw_addr = addr;
				stlink.port.adi.rw_status = STLINK_ADI_REP_FAIL;
			}
			else
			{
				stlink.port.adi.rw_addr = 0;
				stlink.port.adi.rw_status = STLINK_ADI_REP_OK;
			}
			return 0;
		}
		case STLINK_ADI_SUBCMD_READ_MEM8:
		{
			uint32_t addr = GET_LE_U32(&cmd[2]);
			uint16_t len = GET_LE_U16(&cmd[6]);
			
			if (adi_memap_read_buf8(addr, &cmd[0], len))
			{
				stlink.port.adi.rw_addr = addr;
				stlink.port.adi.rw_status = STLINK_ADI_REP_FAIL;
			}
			else
			{
				stlink.port.adi.rw_addr = 0;
				stlink.port.adi.rw_status = STLINK_ADI_REP_OK;
			}
			return len;
		}
		case STLINK_ADI_SUBCMD_WRITE_MEM8:
		{
			uint32_t addr = GET_LE_U32(&cmd[2]);
			uint16_t len = GET_LE_U16(&cmd[6]);
			
			if (adi_memap_write_buf8(addr, &cmd[16], len))
			{
				stlink.port.adi.rw_addr = addr;
				stlink.port.adi.rw_status = STLINK_ADI_REP_FAIL;
			}
			else
			{
				stlink.port.adi.rw_addr = 0;
				stlink.port.adi.rw_status = STLINK_ADI_REP_OK;
			}
			return 0;
		}
		case STLINK_ADI_SUBCMD_EXIT:
		{
			adi_fini();
			return 0;
		}
		case STLINK_ADI_SUBCMD_ENTER:
		{
			struct adi_dpif_t dpif;
			
			// default is JTAG
			if (!cmd[2])
			{
				cmd[2] = STLINK_ADI_ENTER_JTAG;
			}
			
			if (STLINK_ADI_ENTER_JTAG == cmd[2])
			{
				dpif.type = ADI_DP_JTAG;
				dpif.dpif_setting.dpif_jtag_setting.jtag_khz = 9000;
				dpif.dpif_setting.dpif_jtag_setting.jtag_pos.ub = 0;
				dpif.dpif_setting.dpif_jtag_setting.jtag_pos.ua = 1;
				dpif.dpif_setting.dpif_jtag_setting.jtag_pos.bb = 0;
				dpif.dpif_setting.dpif_jtag_setting.jtag_pos.ba = 5;
			}
			else if (STLINK_ADI_ENTER_SWD == cmd[2])
			{
				dpif.type = ADI_DP_SWD;
				dpif.dpif_setting.dpif_swd_setting.swd_trn = 2;
				dpif.dpif_setting.dpif_swd_setting.swd_dly = 0;
				dpif.dpif_setting.dpif_swd_setting.swd_retry = 0;
			}
			else
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_FAIL);
				return 2;
			}
			
			if (cm_switch(&stlink.port.adi.cm_info) ||
				adi_init(interfaces, &dpif, &stlink.port.adi.core))
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_FAIL);
			}
			else
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
			}
			return 2;
		}
		case STLINK_ADI_SUBCMD_READ_IDCODE:
		{
			SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
			SET_LE_U32(&cmd[4], stlink.port.adi.cm_info.adi.dp_info.if_id);
			SET_LE_U32(&cmd[8], 0);
			return 12;
		}
		case STLINK_ADI_SUBCMD_RESET:
		{
			// check result should not be enabled here
			// because after reset, dp maybe disabled
			uint32_t reg32 =
				CM_REG_NVIC_AIRCR_VECTKEY | CM_REG_NVIC_AIRCR_SYSRESETREQ;
			if (adi_memap_write_reg32(CM_REG_NVIC_AIRCR, &reg32, 0) ||
				adi_dp_commit())
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_FAIL);
			}
			else
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
			}
			return 2;
		}
		case STLINK_ADI_SUBCMD_READ_COREREG:
		{
			uint8_t reg_idx = cmd[2];
			uint32_t value32;
			
			if (cm_read_core_register(reg_idx, &value32))
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_FAIL);
			}
			else
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
				SET_LE_U32(&cmd[4], value32);
			}
			return 8;
		}
		case STLINK_ADI_SUBCMD_WRITE_COREREG:
		{
			uint8_t reg_idx = cmd[2];
			uint32_t value32 = GET_LE_U32(&cmd[3]);
			
			if (cm_write_core_register(reg_idx, &value32))
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_FAIL);
			}
			else
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
			}
			return 2;
		}
		case STLINK_ADI_SUBCMD_WRITE_MEMAPREG:
		{
			uint32_t reg_addr = GET_LE_U32(&cmd[2]);
			uint32_t value32 = GET_LE_U32(&cmd[6]);
			
			if (adi_memap_write_reg32(reg_addr, &value32, 1))
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_FAIL);
			}
			else
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
			}
			return 2;
		}
		case STLINK_ADI_SUBCMD_READ_MEMAPREG:
		{
			uint32_t reg_addr = GET_LE_U32(&cmd[2]);
			uint32_t value32;
			
			if (adi_memap_read_reg32(reg_addr, &value32, 1))
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_FAIL);
			}
			else
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
				SET_LE_U32(&cmd[4], value32);
			}
			return 8;
		}
		case STLINK_ADI_SUBCMD_SET_BREAKPOINT:
		{
			uint8_t bp_idx = cmd[2];
			uint32_t bp_addr = GET_LE_U32(&cmd[3]);
			
			if (cm_set_breakpoint(bp_idx, bp_addr))
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_FAIL);
			}
			else
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
			}
			return 2;
		}
		case STLINK_ADI_SUBCMD_CLEAR_BREAKPOINT:
		{
			uint8_t bp_idx = cmd[2];
			
			if (cm_clear_breakpoint(bp_idx))
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_FAIL);
			}
			else
			{
				SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
			}
			return 2;
		}
		case STLINK_ADI_SUBCMD_READ_ALLCOREREGS:
		{
			uint8_t reg_idx;
			uint32_t value32;
			uint8_t result = STLINK_ADI_REP_OK;
			
			for (reg_idx = 0; reg_idx <= 20; reg_idx++)
			{
				if (cm_read_core_register(reg_idx, &value32))
				{
					result = STLINK_ADI_REP_FAIL;
					break;
				}
				else
				{
					SET_LE_U32(&cmd[4 * (reg_idx + 1)], value32);
				}
			}
			SET_LE_U16(&cmd[0], result);
			return 88;
		}
		case STLINK_ADI_SUBCMD_GETRWSTATUS:
		{
			SET_LE_U16(&cmd[0], stlink.port.adi.rw_status);
			return 2;
		}
		case STLINK_ADI_SUBCMD_NRST:
			// TODO: add nrst support
			break;
		case STLINK_ADI_SUBCMD_STATUS:
		{
			SET_LE_U16(&cmd[0], stlink.port.adi.rw_status);
			SET_LE_U32(&cmd[4], stlink.port.adi.rw_addr);
			SET_LE_U32(&cmd[8], 0);
			return 12;
		}
		case STLINK_ADI_SUBCMD_UNKNOWN40:
		{
			// TODO: add unknown40 support
			return 0;
		}
		case STLINK_ADI_SUBCMD_UNKNOWN41:
		{
			// TODO: add unknown41 support
			SET_LE_U16(&cmd[0], STLINK_ADI_REP_OK);
			return 2;
		}
		}
	case STLINK_CMD_DFU:
		// TODO: add DFU support
		switch (cmd[1])
		{
		case STLINK_DFU_SUBCMD_EXIT:
			return 0;
		}
		break;
	case STLINK_CMD_SWIM:
		// TODO: add SWIM support
		break;
	}
	return 0;
}
