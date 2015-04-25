#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "compiler.h"
#include "app_cfg.h"

#include "interfaces.h"
#include "dal/mal/mal.h"
#include "dal/sst32hfxx/sst32hfxx_drv.h"
#include "tool/mal_in_mal/mal_in_mal.h"
#include "tool/mal_embflash/mal_embflash.h"
#include "tool/fakefat32/fakefat32.h"

#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/class/MSC/vsfusbd_MSC_BOT.h"

// mal
// embedded flash
static struct embflash_param_t embflash_param =
{
	0,							// uint8_t index;
};
static struct mal_info_t embflash_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &embflash_drv
};
static struct dal_info_t embflash_dal_info = 
{
	NULL,
	&embflash_param,
	NULL,
	&embflash_mal_info,
};

// firmware, APP_CFG_FWSIZE bytes located at APP_CFG_BOOTSIZE
static struct malinmal_param_t firmware_param =
{
	&embflash_dal_info,			// struct dal_info_t *maldal;
	APP_CFG_BOOTSIZE,			// uint32_t addr;
	APP_CFG_FWSIZE,				// uint32_t size;
};
static struct mal_info_t firmware_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &malinmal_drv
};
static struct dal_info_t firmware_dal_info = 
{
	NULL,
	&firmware_param,
	NULL,
	&firmware_mal_info,
};

#if EVSPROG_EN
// sst32hf164: extnor
static struct sst32hfxx_drv_info_t sst32hfxx_drv_info;
static struct sst32hfxx_drv_param_t sst32hfxx_drv_param;
static struct sst32hfxx_drv_interface_t sst32hfxx_drv_ifs =
{
	0,		// uint8_t ebi_port;
	1,		// uint8_t nor_index;
};
static struct mal_info_t sst32hfxx_mal_info =
{
	{0, 0}, NULL, 0, 0, 0, &sst32hfxx_nor_drv
};
static struct dal_info_t sst32hfxx_dal_info =
{
	&sst32hfxx_drv_ifs,
	&sst32hfxx_drv_param,
	&sst32hfxx_drv_info,
	&sst32hfxx_mal_info,
};

// evsprog_config
static struct malinmal_param_t evsprog_config_param =
{
	&embflash_dal_info,			// struct dal_info_t *maldal;
	EVSPROG_TARGET_CFG_ADDR,	// uint32_t addr;
	EVSPROG_TARGET_CFG_SIZE,	// uint32_t size;
};
static struct mal_info_t evsprog_config_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &malinmal_drv
};
static struct dal_info_t evsprog_config_dal_info = 
{
	NULL,
	&evsprog_config_param,
	NULL,
	&evsprog_config_mal_info,
};

// evsprog_mainscript
static struct malinmal_param_t evsprog_mainscript_param =
{
	&embflash_dal_info,			// struct dal_info_t *maldal;
	EVSPROG_MAINSCRIPT_ADDR,	// uint32_t addr;
	EVSPROG_MAINSCRIPT_SIZE,	// uint32_t size;
};
static struct mal_info_t evsprog_mainscript_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &malinmal_drv
};
static struct dal_info_t evsprog_mainscript_dal_info = 
{
	NULL,
	&evsprog_mainscript_param,
	NULL,
	&evsprog_mainscript_mal_info,
};

// evsprog_target_extnor
static struct malinmal_param_t evsprog_target_extnor_param =
{
	&sst32hfxx_dal_info,			// struct dal_info_t *maldal;
	// addr and size will be initialized according to real flash size
	0,	// uint32_t addr;
	0,	// uint32_t size;
};
static struct mal_info_t evsprog_target_extnor_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &malinmal_drv
};
static struct dal_info_t evsprog_target_extnor_dal_info = 
{
	NULL,
	&evsprog_target_extnor_param,
	NULL,
	&evsprog_target_extnor_mal_info,
};

// evsprog_script_extnor
static struct malinmal_param_t evsprog_script_extnor_param =
{
	&sst32hfxx_dal_info,			// struct dal_info_t *maldal;
	// addr and size will be initialized according to real flash size
	0,	// uint32_t addr;
	0,	// uint32_t size;
};
static struct mal_info_t evsprog_script_extnor_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &malinmal_drv
};
static struct dal_info_t evsprog_script_extnor_dal_info = 
{
	NULL,
	&evsprog_script_extnor_param,
	NULL,
	&evsprog_script_extnor_mal_info,
};

// evsprog_target_embflash
static struct malinmal_param_t evsprog_target_embflash_param =
{
	&embflash_dal_info,			// struct dal_info_t *maldal;
	// addr and size will be initialized according to real flash size
	0,	// uint32_t addr;
	0,	// uint32_t size;
};
static struct mal_info_t evsprog_target_embflash_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &malinmal_drv
};
static struct dal_info_t evsprog_target_embflash_dal_info = 
{
	NULL,
	&evsprog_target_embflash_param,
	NULL,
	&evsprog_target_embflash_mal_info,
};

// evsprog_script_embflash
static struct malinmal_param_t evsprog_script_embflash_param =
{
	&embflash_dal_info,			// struct dal_info_t *maldal;
	// addr and size will be initialized according to real flash size
	0,	// uint32_t addr;
	0,	// uint32_t size;
};
static struct mal_info_t evsprog_script_embflash_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &malinmal_drv
};
static struct dal_info_t evsprog_script_embflash_dal_info = 
{
	NULL,
	&evsprog_script_embflash_param,
	NULL,
	&evsprog_script_embflash_mal_info,
};
#endif		// EVSPROG_EN

// block buffer
static uint8_t block_buffer[4096];

// fakefat32
static const char readme_str[] = 
"\
================================================================================\r\n\
VSF MSCBoot BootLoaddr 0.1beta          COPYRIGHT by SimonQian                  \r\n\
www.versaloon.com                                                               \r\n\
================================================================================\r\n\
\r\n\
Simply update firmware by copying new firmware.bin to the root directory.\r\n\
LOST.DIR directory is used to avoid Android systems creating directory,\r\n\
which is not supported by MSCBoot.\r\n\
\r\n\
By default, the bootloader will take 32KB from the start address of flash.\r\n\
So, your application should start after the bootloader, and firmware.bin should \r\n\
not exceed flash space of the target chip.\r\n\
\r\n\
Note: For VersaloonMini shipped with STM32F103CBT6 and for VersaloonPro, this \r\n\
bootloader is recommended. You can find the bootloader binary under:\r\n\
\trelease/firmware/MiniRelease1/NFW\r\n\
\r\n\
Report to author of Versaloon if any problem when using this bootloader.\r\n\
";

// FAKEFAT32
static vsf_err_t WriteFirmwareArea(struct fakefat32_file_t*file, uint32_t addr,
							uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteFirmwareArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadFirmwareArea(struct fakefat32_file_t*file, uint32_t addr,
							uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadFirmwareArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadInfo(struct fakefat32_file_t*file, uint32_t addr,
							uint8_t *buff, uint32_t page_size);
#if EVSPROG_EN
static vsf_err_t WriteConfigArea(struct fakefat32_file_t*file, uint32_t addr,
							uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteConfigArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadConfigArea(struct fakefat32_file_t*file, uint32_t addr,
							uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadConfigArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteMainScriptArea(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteMainScriptArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadMainScriptArea(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadMainScriptArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ChangeMainScriptAreaSize(struct fakefat32_file_t *file,
							uint32_t size);
static vsf_err_t WriteScriptAreaExtNor(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteScriptAreaExtNor_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadScriptAreaExtNor(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadScriptAreaExtNor_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ChangeScriptAreaSizeExtNor(struct fakefat32_file_t *file,
											uint32_t size);
static vsf_err_t WriteTargetAreaExtNor(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteTargetAreaExtNor_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadTargetAreaExtNor(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadTargetAreaExtNor_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteTargetAreaEmbFlash(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteTargetAreaEmbFlash_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadTargetAreaEmbFlash(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadTargetAreaEmbFlash_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteScriptAreaEmbFlash(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t WriteScriptAreaEmbFlash_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadScriptAreaEmbFlash(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ReadScriptAreaEmbFlash_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size);
static vsf_err_t ChangeScriptAreaSizeEmbFlash(struct fakefat32_file_t *file,
							uint32_t size);
#endif
static struct fakefat32_file_t lost_dir[] =
{
	{
		".", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"..", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		NULL,
	}
};

#if EVSPROG_EN
#define TARGET_BIN_IDX				2
#define TARGET_SCRIPT_IDX			3
static struct fakefat32_file_t target_slot_embflash_dir[] =
{
	{
		".", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"..", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"target", "bin",
		FAKEFAT32_FILEATTR_ARCHIVE,
		512,
		{
			ReadTargetAreaEmbFlash,
			ReadTargetAreaEmbFlash_isready,
			WriteTargetAreaEmbFlash,
			WriteTargetAreaEmbFlash_isready
		},
	},
	{
		"script", "txt",
		FAKEFAT32_FILEATTR_ARCHIVE,
		512,
		{
			ReadScriptAreaEmbFlash,
			ReadScriptAreaEmbFlash_isready,
			WriteScriptAreaEmbFlash,
			WriteScriptAreaEmbFlash_isready,
			ChangeScriptAreaSizeEmbFlash
		},
	},
	{
		NULL,
	}
};

static struct fakefat32_file_t target_slot_extnor_dir[] =
{
	{
		".", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"..", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"target", "bin",
		FAKEFAT32_FILEATTR_ARCHIVE,
		512,
		{
			ReadTargetAreaExtNor,
			ReadTargetAreaExtNor_isready,
			WriteTargetAreaExtNor,
			WriteTargetAreaExtNor_isready
		},
	},
	{
		"script", "txt",
		FAKEFAT32_FILEATTR_ARCHIVE,
		512,
		{
			ReadScriptAreaExtNor,
			ReadScriptAreaExtNor_isready,
			WriteScriptAreaExtNor,
			WriteScriptAreaExtNor_isready,
			ChangeScriptAreaSizeExtNor
		},
	},
	{
		NULL,
	}
};
#endif

#define ROOT_FIRMWARE_IDX			3
#define ROOT_CONFIG_IDX				4
#define ROOT_MAINSCRIPT_IDX			5
#define ROOT_TARGETS_IDX			6
#if EVSPROG_EN
static char faktfat32_filename_slotn[] = "slotn";
#endif
static struct fakefat32_file_t root_dir[] =
{
	{
		"MSCBoot", NULL,
		FKAEFAT32_FILEATTR_VOLUMEID,
	},
	{
		"LOST", "DIR",
		FAKEFAT32_FILEATTR_DIRECTORY,
		0,
		{
			fakefat32_dir_read,
			NULL,
			fakefat32_dir_write,
			NULL
		},
		lost_dir
	},
	{
		"readme", "txt",
		FAKEFAT32_FILEATTR_ARCHIVE | FAKEFAT32_FILEATTR_READONLY,
		sizeof(readme_str) - 1,
		{ReadInfo},
	},
	{
		"firmware", "bin",
		FAKEFAT32_FILEATTR_ARCHIVE,
		512,
		{
			ReadFirmwareArea,
			ReadFirmwareArea_isready,
			WriteFirmwareArea,
			WriteFirmwareArea_isready
		},
	},
#if EVSPROG_EN
	{
		"config", "bin",
		FAKEFAT32_FILEATTR_ARCHIVE,
		512,
		{
			ReadConfigArea,
			ReadConfigArea_isready,
			WriteConfigArea,
			WriteConfigArea_isready
		},
	},
	{
		"script", "txt",
		FAKEFAT32_FILEATTR_ARCHIVE,
		512,
		{
			ReadMainScriptArea,
			ReadMainScriptArea_isready,
			WriteMainScriptArea,
			WriteMainScriptArea_isready,
			ChangeMainScriptAreaSize
		},
	},
	{
		NULL, NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
		0,
		{
			fakefat32_dir_read,
			NULL,
			fakefat32_dir_write,
			NULL
		},
	},
	{
		NULL, NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
		0,
		{
			fakefat32_dir_read,
			NULL,
			fakefat32_dir_write,
			NULL
		},
		NULL
	},
#endif
	{
		NULL,
	}
};
static struct fakefat32_param_t fakefat32_param =
{
	512,			// uint16_t sector_size;
	0x00760000,		// uint32_t sector_number;
	8,				// uint8_t sectors_per_cluster;
	
	0x0CA93E47,		// uint32_t volume_id;
	0x12345678,		// uint32_t disk_id;
	{				// struct fakefat32_file_t root;
		{
			"ROOT", NULL,
			0,
			0,
			{fakefat32_dir_read, NULL, fakefat32_dir_write, NULL},
			root_dir
		}
	}
};
static struct mal_info_t fakefat32_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &fakefat32_drv
};
static struct dal_info_t fakefat32_dal_info = 
{
	NULL,
	&fakefat32_param,
	NULL,
	&fakefat32_mal_info,
};

static vsf_err_t ReadInfo(struct fakefat32_file_t*file, uint32_t addr,
									uint8_t *buff, uint32_t page_size)
{
	if (addr < (sizeof(readme_str) - 1))
	{
		uint32_t remain_size = sizeof(readme_str) - 1 - addr;
		memcpy(buff, &readme_str[addr], min(remain_size, page_size));
	}
	else
	{
		memset(buff, 0xFF, page_size);
	}
	return VSFERR_NONE;
}

enum mal_io_fsm_t
{
	MAL_IO_IDLE,
	MAL_IO_ERASE,
	MAL_IO_WRITE,
	MAL_IO_READ,
} static mal_io_fsm = MAL_IO_IDLE;
static uint32_t mal_io_cnt, mal_io_curcnt;
static vsf_err_t MalEraseWrite(struct dal_info_t *info, uint32_t addr,
								uint8_t *buff, uint32_t page_size)
{
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t mal_pagesize = (uint32_t)mal_info->capacity.block_size;
	uint32_t mal_pagenum = (uint32_t)mal_info->capacity.block_number;
	
	if ((0 == mal_pagesize) || (page_size < mal_pagesize) ||
		(addr >= (mal_pagesize * mal_pagenum)))
	{
		return VSFERR_FAIL;
	}
	mal_io_curcnt = 0;
	mal_io_cnt = page_size / mal_pagesize;
	
	mal_io_fsm = MAL_IO_ERASE;
	mal.eraseblock_nb_start(info, addr, mal_io_cnt);
	return mal.eraseblock_nb(info, addr);
}

static vsf_err_t MalEraseWrite_isready(struct dal_info_t *info, uint32_t addr,
										uint8_t *buff, uint32_t page_size)
{
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t mal_pagesize = (uint32_t)mal_info->capacity.block_size;
	vsf_err_t err;
	
	switch (mal_io_fsm)
	{
	case MAL_IO_ERASE:
		err = mal.eraseblock_nb_isready(info,
										addr + mal_pagesize * mal_io_curcnt);
		if (err)
		{
			return err;
		}
		mal_io_curcnt++;
		
		if (mal_io_curcnt >= mal_io_cnt)
		{
			mal.eraseblock_nb_end(info);
			mal_io_fsm = MAL_IO_WRITE;
			mal_io_curcnt = 0;
			if (mal.writeblock_nb_start(info, addr, mal_io_cnt, buff) ||
				mal.writeblock_nb(info, addr, buff))
			{
				return VSFERR_FAIL;
			}
			return VSFERR_NOT_READY;
		}
		else
		{
			err = mal.eraseblock_nb(info, addr + mal_pagesize * mal_io_curcnt);
			if (err)
			{
				return err;
			}
			return VSFERR_NOT_READY;
		}
	case MAL_IO_WRITE:
		err = mal.writeblock_nb_isready(info,
									addr + mal_pagesize * mal_io_curcnt,
									buff + mal_pagesize * mal_io_curcnt);
		if (err)
		{
			return err;
		}
		mal_io_curcnt++;
		
		if (mal_io_curcnt >= mal_io_cnt)
		{
			mal.writeblock_nb_end(info);
			mal_io_fsm = MAL_IO_IDLE;
			return VSFERR_NONE;
		}
		else
		{
			err = mal.writeblock_nb(info,
									addr + mal_pagesize * mal_io_curcnt,
									buff + mal_pagesize * mal_io_curcnt);
			if (err)
			{
				return err;
			}
			return VSFERR_NOT_READY;
		}
	default:
		return VSFERR_FAIL;
	}
}

static vsf_err_t MalRead(struct dal_info_t *info, uint32_t addr, uint8_t *buff,
							uint32_t page_size)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(page_size);
	return VSFERR_NONE;
}

static vsf_err_t MalRead_isready(struct dal_info_t *info, uint32_t addr,
									uint8_t *buff, uint32_t page_size)
{
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t mal_pagesize = (uint32_t)mal_info->capacity.block_size;
	uint32_t mal_pagenum = (uint32_t)mal_info->capacity.block_number;
	vsf_err_t err;
	
	switch (mal_io_fsm)
	{
	case MAL_IO_IDLE:
		if ((0 == mal_pagesize) || (page_size < mal_pagesize) ||
			(addr >= (mal_pagesize * mal_pagenum)))
		{
			return VSFERR_FAIL;
		}
		mal_io_curcnt = 0;
		mal_io_cnt = page_size / mal_pagesize;
		mal.readblock_nb_start(info, addr, mal_io_cnt, buff);
		mal_io_fsm = MAL_IO_READ;
	case MAL_IO_READ:
		err = mal.readblock_nb_isready(info, addr + mal_pagesize * mal_io_curcnt,
										buff + mal_pagesize * mal_io_curcnt);
		if (err)
		{
			return err;
		}
		err = mal.readblock_nb(info, addr + mal_pagesize * mal_io_curcnt,
								buff + mal_pagesize * mal_io_curcnt);
		if (err)
		{
			return err;
		}
		if (++mal_io_curcnt >= mal_io_cnt)
		{
			mal.readblock_nb_end(info);
			mal_io_fsm = MAL_IO_IDLE;
			return VSFERR_NONE;
		}
		return VSFERR_NOT_READY;
	default:
		return VSFERR_FAIL;
	}
}

static vsf_err_t WriteFirmwareArea(struct fakefat32_file_t* file, uint32_t addr,
									uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite(&firmware_dal_info, addr, buff, page_size);
}

static vsf_err_t WriteFirmwareArea_isready(struct fakefat32_file_t* file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite_isready(&firmware_dal_info, addr, buff, page_size);
}

static vsf_err_t ReadFirmwareArea(struct fakefat32_file_t* file, uint32_t addr,
									uint8_t *buff, uint32_t page_size)
{
#if APP_CFG_MSC_WRITEONLY
	memset(buff, 0xFF, page_size);
	return VSFERR_NONE;
#else
	return MalRead(&firmware_dal_info, addr, buff, page_size);
#endif
}

static vsf_err_t ReadFirmwareArea_isready(struct fakefat32_file_t* file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
#if APP_CFG_MSC_WRITEONLY
	return VSFERR_NONE;
#else
	return MalRead_isready(&firmware_dal_info, addr, buff, page_size);
#endif
}

#if EVSPROG_EN
struct app_cfg_t
{
	uint16_t main_script_size;
	uint16_t slot_script_size[2];
};
static int8_t slot_idx_embflash, slot_idx_extnor;
static struct app_cfg_t app_cfg;
static vsf_err_t ReadAppCfg(struct app_cfg_t *cfg)
{
	uint32_t cfg_addr;
	
	cfg_addr = (uint32_t)(APP_CFG_BOOTSIZE - embflash_mal_info.capacity.block_size);
	if (mal.readblock(&embflash_dal_info, cfg_addr, block_buffer, 1))
	{
		return VSFERR_FAIL;
	}
	memcpy(&app_cfg, block_buffer, sizeof(app_cfg));
	return VSFERR_NONE;
}
static vsf_err_t WriteAppCfg(struct app_cfg_t *cfg)
{
	uint32_t cfg_addr;
	
	cfg_addr = (uint32_t)(APP_CFG_BOOTSIZE - embflash_mal_info.capacity.block_size);
	memcpy(block_buffer, &app_cfg, sizeof(app_cfg));
	return mal.writeblock(&embflash_dal_info, cfg_addr, block_buffer, 1);
}

static vsf_err_t WriteConfigArea(struct fakefat32_file_t*file, uint32_t addr,
							uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite(&evsprog_config_dal_info, addr, buff, page_size);
}
static vsf_err_t WriteConfigArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite_isready(&evsprog_config_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadConfigArea(struct fakefat32_file_t*file, uint32_t addr,
							uint8_t *buff, uint32_t page_size)
{
	return MalRead(&evsprog_config_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadConfigArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalRead_isready(&evsprog_config_dal_info, addr, buff, page_size);
}
static vsf_err_t WriteMainScriptArea(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite(&evsprog_mainscript_dal_info, addr, buff, page_size);
}
static vsf_err_t WriteMainScriptArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite_isready(&evsprog_mainscript_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadMainScriptArea(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalRead(&evsprog_mainscript_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadMainScriptArea_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalRead_isready(&evsprog_mainscript_dal_info, addr, buff, page_size);
}
static vsf_err_t ChangeMainScriptAreaSize(struct fakefat32_file_t *file,
											uint32_t size)
{
	if (size > EVSPROG_MAINSCRIPT_SIZE)
	{
		return VSFERR_FAIL;
	}
	app_cfg.main_script_size = size;
	return WriteAppCfg(&app_cfg);
}

static vsf_err_t WriteScriptAreaExtNor(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite(&evsprog_script_extnor_dal_info, addr, buff, page_size);
}
static vsf_err_t WriteScriptAreaExtNor_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite_isready(&evsprog_script_extnor_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadScriptAreaExtNor(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalRead(&evsprog_script_extnor_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadScriptAreaExtNor_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalRead_isready(&evsprog_script_extnor_dal_info, addr, buff, page_size);
}
static vsf_err_t ChangeScriptAreaSizeExtNor(struct fakefat32_file_t *file,
							uint32_t size)
{
	if ((slot_idx_extnor < 0) || (size > EVSPROG_MAINSCRIPT_SIZE))
	{
		return VSFERR_FAIL;
	}
	app_cfg.slot_script_size[slot_idx_extnor] = size;
	return WriteAppCfg(&app_cfg);
}

static vsf_err_t WriteTargetAreaExtNor(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite(&evsprog_target_extnor_dal_info, addr, buff, page_size);
}
static vsf_err_t WriteTargetAreaExtNor_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite_isready(&evsprog_target_extnor_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadTargetAreaExtNor(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
#if APP_CFG_MSC_WRITEONLY
	memset(buff, 0xFF, page_size);
	return VSFERR_NONE;
#else
	return MalRead(&evsprog_target_extnor_dal_info, addr, buff, page_size);
#endif
}
static vsf_err_t ReadTargetAreaExtNor_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
#if APP_CFG_MSC_WRITEONLY
	return VSFERR_NONE;
#else
	return MalRead_isready(&evsprog_target_extnor_dal_info, addr, buff, page_size);
#endif
}

static vsf_err_t WriteTargetAreaEmbFlash(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite(&evsprog_target_embflash_dal_info, addr, buff, page_size);
}
static vsf_err_t WriteTargetAreaEmbFlash_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite_isready(&evsprog_target_embflash_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadTargetAreaEmbFlash(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
#if APP_CFG_MSC_WRITEONLY
	memset(buff, 0xFF, page_size);
	return VSFERR_NONE;
#else
	return MalRead(&evsprog_target_embflash_dal_info, addr, buff, page_size);
#endif
}
static vsf_err_t ReadTargetAreaEmbFlash_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
#if APP_CFG_MSC_WRITEONLY
	return VSFERR_NONE;
#else
	return MalRead_isready(&evsprog_target_embflash_dal_info, addr, buff, page_size);
#endif
}

static vsf_err_t WriteScriptAreaEmbFlash(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite(&evsprog_script_embflash_dal_info, addr, buff, page_size);
}
static vsf_err_t WriteScriptAreaEmbFlash_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalEraseWrite_isready(&evsprog_script_embflash_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadScriptAreaEmbFlash(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalRead(&evsprog_script_embflash_dal_info, addr, buff, page_size);
}
static vsf_err_t ReadScriptAreaEmbFlash_isready(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	return MalRead_isready(&evsprog_script_embflash_dal_info, addr, buff, page_size);
}
static vsf_err_t ChangeScriptAreaSizeEmbFlash(struct fakefat32_file_t *file,
							uint32_t size)
{
	if ((slot_idx_embflash < 0) || (size > EVSPROG_MAINSCRIPT_SIZE))
	{
		return VSFERR_FAIL;
	}
	app_cfg.slot_script_size[slot_idx_embflash] = size;
	return WriteAppCfg(&app_cfg);
}
#endif

// USB
static const uint8_t MSCBOT_DeviceDescriptor[] =
{
	0x12,	// bLength
	USB_DESC_TYPE_DEVICE,	 // bDescriptorType
	0x00,
	0x02,	// bcdUSB = 2.00
	0, 0, 0,
	0x40,	// bMaxPacketSize0
	0x83,
	0x04,	// idVendor = 0x0483
	0x20,
	0x57,	// idProduct = 0x5720
	0x00,
	0x01,	// bcdDevice = 1.00
	1,		// Index of string descriptor describing manufacturer
	2,		// Index of string descriptor describing product
	3,		// Index of string descriptor describing the device's serial number
	0x01	// bNumConfigurations
};

static const uint8_t MSCBOT_ConfigDescriptor[] =
{
	// Configuation Descriptor
	0x09,	// bLength: Configuation Descriptor size
	USB_DESC_TYPE_CONFIGURATION,
			// bDescriptorType: Configuration
	32,		// wTotalLength:no of returned bytes
	0x00,
	0x01,	// bNumInterfaces: 1 interface
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x64,	// MaxPower 200 mA

	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	0x04,	// bDescriptorType:
	0x00,	// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints
	0x08,	// bInterfaceClass: MASS STORAGE Class
	0x06,	// bInterfaceSubClass : SCSI transparent
	0x50,	// nInterfaceProtocol
	0x00,	// iInterface:

	// Endpoint 1 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x81,	// Endpoint address (IN, address 1)
	0x02,	// Bulk endpoint type
	0x40,	// Maximum packet size (64 bytes)
	0x00,
	0x00,	// Polling interval in milliseconds

	// Endpoint 1 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x01,	// Endpoint address (OUT, address 1)
	0x02,	// Bulk endpoint type
	0x40,	// Maximum packet size (64 bytes)
	0x00,
	0x00,	// Polling interval in milliseconds
};

static const uint8_t MSCBOT_StringLangID[] =
{
	4,
	USB_DESC_TYPE_STRING,
	0x09,
	0x04
};

static const uint8_t MSCBOT_StringVendor[] =
{
	38,
	USB_DESC_TYPE_STRING,
	'S', 0, 'T', 0, 'M', 0, 'i', 0, 'c', 0, 'r', 0, 'o', 0, 'e', 0,
	'l', 0, 'e', 0, 'c', 0, 't', 0, 'r', 0, 'o', 0, 'n', 0, 'i', 0,
	'c', 0, 's', 0
};

static const uint8_t MSCBOT_StringProduct[] =
{
	16,
	USB_DESC_TYPE_STRING,
	'M', 0, 'S', 0, 'C', 0, 'B', 0, 'o', 0, 'o', 0, 't', 0
};

static const uint8_t MSCBOT_StringSerial[50] =
{
	50,
	USB_DESC_TYPE_STRING,
	'0', 0, '1', 0, '2', 0, '3', 0, '4', 0, '5', 0, '6', 0, '7', 0, 
	'8', 0, '9', 0, 'A', 0, 'B', 0, 'C', 0, 'D', 0, 'E', 0, 'F', 0, 
	'0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, 
};

static const struct vsfusbd_desc_filter_t descriptors[] = 
{
	VSFUSBD_DESC_DEVICE(0, MSCBOT_DeviceDescriptor, sizeof(MSCBOT_DeviceDescriptor), NULL),
	VSFUSBD_DESC_CONFIG(0, 0, MSCBOT_ConfigDescriptor, sizeof(MSCBOT_ConfigDescriptor), NULL),
	VSFUSBD_DESC_STRING(0, 0, MSCBOT_StringLangID, sizeof(MSCBOT_StringLangID), NULL),
	VSFUSBD_DESC_STRING(0x0409, 1, MSCBOT_StringVendor, sizeof(MSCBOT_StringVendor), NULL),
	VSFUSBD_DESC_STRING(0x0409, 2, MSCBOT_StringProduct, sizeof(MSCBOT_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 3, MSCBOT_StringSerial, sizeof(MSCBOT_StringSerial), NULL),
	VSFUSBD_DESC_NULL
};

struct SCSI_LUN_info_t MSCBOT_LunInfo = 
{
	&fakefat32_dal_info, 
	{
		true,
		{'S', 'i', 'm', 'o', 'n', ' ', ' ', ' '},
		{'M', 'S', 'C', 'B', 'o', 'o', 't', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
		{'1', '.', '0', '0'},
		SCSI_PDT_DIRECT_ACCESS_BLOCK
	}
};
uint8_t MSCBOT_Buffer0[4096], MSCBOT_Buffer1[4096];

struct vsfusbd_MSCBOT_param_t MSCBOT_param = 
{
	1,							// uint8_t ep_out;
	1,							// uint8_t ep_in;
	
	0,							// uint8_t max_lun;
	&MSCBOT_LunInfo,			// struct SCSI_LUN_info_t *lun_info;
	NULL, 						// struct SCSI_handler_t *user_handlers;
	
	{
		{MSCBOT_Buffer0, sizeof(MSCBOT_Buffer0)},
		{MSCBOT_Buffer1, sizeof(MSCBOT_Buffer1)}
	},							// struct vsf_buffer_t page_buffer[2];
};

static struct vsfusbd_iface_t ifaces[] = 
{
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_MSCBOT_class, (void *)&MSCBOT_param},
	{(struct vsfusbd_class_protocol_t *)NULL, (void *)NULL}
};
static struct vsfusbd_config_t config0[] = 
{
	{
		NULL, NULL, dimof(ifaces), (struct vsfusbd_iface_t *)ifaces,
	}
};
struct vsfusbd_device_t usb_device = 
{
	1, (struct vsfusbd_config_t *)config0, 
	(struct vsfusbd_desc_filter_t *)descriptors, 0, 
	(struct interface_usbd_t *)&core_interfaces.usbd,
	
	{
		NULL,			// init
		NULL,			// fini
		NULL,			// poll
		NULL,			// on_set_interface
		
		NULL,			// on_ATTACH
		NULL,			// on_DETACH
		NULL,			// on_RESET
		NULL,			// on_ERROR
		NULL,			// on_WAKEUP
		NULL,			// on_SUSPEND
		NULL,			// on_RESUME
		NULL,			// on_SOF
		
		NULL,			// on_IN
		NULL,			// on_OUT
	},			// callback
};

static uint32_t MSP, RST_VECT;
static void fatal_error(void)
{
	while(1);
}
int main(void)
{
	uint32_t pagesize, pagenum, size;
	uint32_t key_val;
	
	interfaces->core.init(NULL);
	if (!interfaces->flash.isprotected(0))
	{
		interfaces->flash.unlock(0);
		interfaces->flash.protect(0);
		interfaces->flash.lock(0);
	}
	
	if (mal.init(&embflash_dal_info) ||
		(0 == embflash_mal_info.capacity.block_size) ||
		(sizeof(block_buffer) < embflash_mal_info.capacity.block_size) ||
		mal.readblock(&embflash_dal_info, APP_CFG_BOOTSIZE, block_buffer, 1))
	{
		fatal_error();
	}
	pagesize = (uint32_t)embflash_mal_info.capacity.block_size;
	pagenum = (uint32_t)embflash_mal_info.capacity.block_number;
	size = pagesize * pagenum;
	
	// read MSP and RST_VECT
	MSP = GET_LE_U32(&block_buffer[0]);
	RST_VECT = GET_LE_U32(&block_buffer[4]);
	
	interfaces->gpio.init(KEY_PORT);
	interfaces->gpio.config_pin(KEY_PORT, KEY_PIN, KEY_VALID_LOW ? GPIO_INPU : GPIO_INPD);
	key_val = interfaces->gpio.get(KEY_PORT, 1 << KEY_PIN);
	if ((KEY_VALID_LOW ? key_val : !key_val) &&
		((MSP & 0xFF000000) == 0x20000000) &&
		((RST_VECT & 0xFF000000) == 0x08000000))
	{
		mal.fini(&embflash_dal_info);
		interfaces->gpio.fini(KEY_PORT);
		interfaces->core.set_stack(MSP);
		((void (*)(void))RST_VECT)();
		while (1);
	}
	
	interfaces->gpio.init(USB_PULL_PORT);
	// Disable USB Pull-up
	interfaces->gpio.clear(USB_PULL_PORT, 1 << USB_PULL_PIN);
	interfaces->gpio.config_pin(USB_PULL_PORT, USB_PULL_PIN, GPIO_OUTPP);
	// delay
	interfaces->delay.delayms(200);
	
	// fixes size for firmware.bin, config.bin, script.txt
#if EVSPROG_EN
	if (size < (APP_CFG_BOOTSIZE + APP_CFG_FWSIZE + EVSPROG_TARGET_CFG_SIZE + EVSPROG_MAINSCRIPT_SIZE))
	{
		fatal_error();
	}
	ReadAppCfg(&app_cfg);
	if ((app_cfg.main_script_size > EVSPROG_MAINSCRIPT_SIZE) ||
		(app_cfg.slot_script_size[0] > EVSPROG_TARGETSCRIPT_SIZE) ||
		(app_cfg.slot_script_size[1] > EVSPROG_TARGETSCRIPT_SIZE))
	{
		// initialize app_cfg
		memset(&app_cfg, 0, sizeof(app_cfg));
		WriteAppCfg(&app_cfg);
	}
	// firmware.bin
	root_dir[ROOT_FIRMWARE_IDX].size = APP_CFG_FWSIZE;
	// config.bin
	root_dir[ROOT_CONFIG_IDX].size = EVSPROG_MAINSCRIPT_ADDR - EVSPROG_TARGET_CFG_ADDR;
	// mainscript
	root_dir[ROOT_MAINSCRIPT_IDX].size = app_cfg.main_script_size;
	if (size > (APP_CFG_BOOTSIZE + APP_CFG_FWSIZE + EVSPROG_TARGET_CFG_SIZE + EVSPROG_MAINSCRIPT_SIZE))
	{
		// slot0 is embflash
		slot_idx_embflash = 0;
		evsprog_target_embflash_param.addr = APP_CFG_BOOTSIZE + APP_CFG_FWSIZE + EVSPROG_TARGET_CFG_SIZE + EVSPROG_MAINSCRIPT_SIZE;
		evsprog_target_embflash_param.size = size - evsprog_target_embflash_param.addr - EVSPROG_TARGETSCRIPT_SIZE;
		evsprog_script_embflash_param.addr = evsprog_target_embflash_param.addr + evsprog_target_embflash_param.size;
		evsprog_script_embflash_param.size = EVSPROG_TARGETSCRIPT_SIZE;
		target_slot_embflash_dir[TARGET_BIN_IDX].size = evsprog_target_embflash_param.size;
		target_slot_embflash_dir[TARGET_SCRIPT_IDX].size = app_cfg.slot_script_size[slot_idx_embflash];
		root_dir[ROOT_TARGETS_IDX + slot_idx_embflash].name = "slot0";
		root_dir[ROOT_TARGETS_IDX + slot_idx_embflash].filelist = target_slot_embflash_dir;
	}
	else
	{
		slot_idx_embflash = -1;
	}
	// sst32hfxx parameter init
	sst32hfxx_drv_param.nor_info.common_info.data_width = 16;
	sst32hfxx_drv_param.nor_info.common_info.wait_signal = EBI_WAIT_NONE;
	sst32hfxx_drv_param.nor_info.param.addr_multiplex = false;
	sst32hfxx_drv_param.nor_info.param.timing.clock_hz_r = 
		sst32hfxx_drv_param.nor_info.param.timing.clock_hz_w = 0;
	sst32hfxx_drv_param.nor_info.param.timing.address_setup_cycle_r = 
		sst32hfxx_drv_param.nor_info.param.timing.address_setup_cycle_w = 2;
	sst32hfxx_drv_param.nor_info.param.timing.address_hold_cycle_r = 
		sst32hfxx_drv_param.nor_info.param.timing.address_hold_cycle_w = 0;
	sst32hfxx_drv_param.nor_info.param.timing.data_setup_cycle_r = 
		sst32hfxx_drv_param.nor_info.param.timing.data_setup_cycle_w = 16;
	sst32hfxx_drv_param.delayus = 20;
	sst32hfxx_mal_info.capacity.block_size = 4096;
	sst32hfxx_mal_info.capacity.block_number = 512;
	if (!mal.init(&sst32hfxx_dal_info))
	{
		uint32_t extnor_pagesize = sst32hfxx_mal_info.capacity.block_size;
		uint32_t extnor_size = (uint32_t)(
					extnor_pagesize * sst32hfxx_mal_info.capacity.block_number);
		
		// fix pagesize for fakefat32
		pagesize = max(extnor_pagesize, pagesize);
		
		slot_idx_extnor = 0;
		if (slot_idx_embflash >= 0)
		{
			slot_idx_extnor = slot_idx_embflash + 1;
		}
		faktfat32_filename_slotn[4] = '0' + slot_idx_extnor;
		// initialize extnor
		evsprog_target_extnor_param.addr = 0;
		evsprog_target_extnor_param.size = extnor_size - EVSPROG_TARGETSCRIPT_SIZE;
		evsprog_script_extnor_param.addr = evsprog_target_extnor_param.size;
		evsprog_script_extnor_param.size = EVSPROG_TARGETSCRIPT_SIZE;
		target_slot_extnor_dir[TARGET_BIN_IDX].size = evsprog_target_extnor_param.size;
		target_slot_extnor_dir[TARGET_SCRIPT_IDX].size = app_cfg.slot_script_size[slot_idx_extnor];
		root_dir[ROOT_TARGETS_IDX + slot_idx_extnor].name = faktfat32_filename_slotn;
		root_dir[ROOT_TARGETS_IDX + slot_idx_extnor].filelist = target_slot_extnor_dir;
	}
	else
	{
		slot_idx_extnor = -1;
	}
#else
	// firmware.bin
	root_dir[ROOT_FIRMWARE_IDX].size = size - APP_CFG_BOOTSIZE;
#endif
	mal.init(&firmware_dal_info);
#if EVSPROG_EN
	mal.init(&evsprog_config_dal_info);
	mal.init(&evsprog_mainscript_dal_info);
	mal.init(&evsprog_target_extnor_dal_info);
	mal.init(&evsprog_script_extnor_dal_info);
	mal.init(&evsprog_target_embflash_dal_info);
	mal.init(&evsprog_script_embflash_dal_info);
#endif
	// fakefat32 parameter init
	fakefat32_param.sector_size = pagesize;
	fakefat32_param.sector_number = 128 * 1024 * 1024 / pagesize;
	fakefat32_param.sectors_per_cluster = 1;
	mal.init(&fakefat32_dal_info);
	
	// Enable USB Pull-up
	interfaces->gpio.set(USB_PULL_PORT, 1 << USB_PULL_PIN);
	
	if (!vsfusbd_device_init(&usb_device))
	{
		while (1)
		{
			if (vsfusbd_device_poll(&usb_device))
			{
				break;
			}
		}
	}
	
	return 0;
}
