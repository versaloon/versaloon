/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef __APP_ERR_H_INCLUDED__
#define __APP_ERR_H_INCLUDED__

// Common error messages
#define ERRMSG_TRY_HELP						"Try add '--help' or '-h' for more information."
#define ERRMSG_TRY_SUPPORT					"Try add '--support' or '-S' for more information."
#define ERRMSG_MUTIPLE_DEFINED				"Mutiple %s defined."
#define ERRMSG_NOT_DEFINED					"%s is not defined, please define first."
#define ERRMSG_BUFFER_OVERFLOW				"buffer %s overflow"
#define ERRMSG_NOT_INITIALIZED				"%s %s is not initialized"
#define ERRMSG_TIMEOUT						"timeout to %s"

#define ERRMSG_NOT_ENOUGH_MEMORY			"Lack of memory."

#define ERRMSG_INVALID						"%s is invalide for %s."
#define ERRMSG_INVALID_CHARACTER			"%c is invalide for %s."
#define ERRMSG_INVALID_CHARACTER_MESSAGE	"%c is invalide for %s, %s"
#define ERRMSG_INVALID_VALUE				"%d is invalid for %s."
#define ERRMSG_INVALID_VALUE_MESSAGE		"%d is invalid for %s, %s"
#define ERRMSG_INVALID_HEX					"0x%X is invalid for %s."
#define ERRMSG_INVALID_HEX_MESSAGE			"0x%X is invalid for %s, %s"
#define ERRMSG_INVALID_ADDRESS				"Address 0x%X is invalid for %s."
#define ERRMSG_INVALID_INDEX				"Index %d is invalid for %s."
#define ERRMSG_INVALID_RANGE				"Invalid range for %s."
#define ERRMSG_INVALID_USAGE				"Invalid usage of %s"
#define ERRMSG_INVALID_TARGET				"Invalid %s"
#define ERRCODE_INVALID						VSFERR_FAIL

#define ERRMSG_INVALID_OPTION				"Invalid option: %c."
#define ERRMSG_INVALID_OPERATION			"Invalid operation: %c."
#define ERRMSG_INVALID_CMD					"Invalid cmd: %s."
#define ERRCODE_INVALID_OPTION				VSFERR_FAIL

#define ERRMSG_INVALID_PARAMETER			"Invalid parameter of %s."

#define ERRMSG_NOT_SUPPORT					"%s is not supported."
#define ERRMSG_NOT_SUPPORT_BY				"%s is not supported by %s."
#define ERRMSG_NOT_SUPPORT_AS				"%s is not supported as %s."

#define ERRMSG_INVALID_BUFFER				"Buffer %s is not valid."
#define ERRCODE_INVALID_BUFFER				VSFERR_FAIL

#define ERRMSG_INVALID_HANDLER				"%s is not valid handler for %s."
#define ERRCODE_INVALID_HANDLER				VSFERR_FAIL

#define ERRMSG_FAILURE_OPERATION			"Fail to %s."
#define ERRMSG_FAILURE_ERASE				"Fail to erase %s."
#define ERRMSG_FAILURE_PROGRAM				"Fail to program %s."
#define ERRMSG_FAILURE_READ					"Fail to read %s."
#define ERRMSG_FAILURE_VERIFY				"Fail to verify %s."
#define ERRMSG_FAILURE_OPERATION_ERRCODE	"Fail to %s, error code is %d."
#define ERRMSG_FAILURE_OPERATION_ERRCODE16	"Fail to %s, error code is 0x%X."
#define ERRMSG_FAILURE_OPERATION_ERRSTRING	"Fail to %s, error string is %s."
#define ERRMSG_FAILURE_OPERATION_MESSAGE	"Fail to %s, %s"
#define ERRMSG_FAILURE_OPERATE_DEVICE		"Fail to operate %s."
#define ERRMSG_FAILURE_HANDLE_DEVICE		"Fail to %s %s."
#define ERRMSG_FAILURE_OPERATE_ADDRESS		"Fail to %s at 0x%08X"
#define ERRCODE_FAILURE_OPERATION			VSFERR_FAIL

#define ERRMSG_FAILURE_OPEN					"Fail to open %s."
#define ERRCODE_FAILURE_OPEN				VSFERR_FAIL





// User defined error messages
#define ERRMSG_INVALID_INTERFACE_NUM		"invalid inteface %d"

#define ERRMSG_AUTODETECT_FAIL				"%s auto-detect failed."
#define ERRCODE_AUTODETECT_FAIL				VSFERR_FAIL

#define ERRMSG_INVALID_TGTCFG_SETTING		"Invalid setting in config file for %s"
#define ERRMSG_INVALID_PROG_MODE			"Program mode %d is invalid for %s."
#define ERRCODE_INVALID_PROG_MODE			VSFERR_FAIL

#define ERRMSG_FAILURE_ENTER_PROG_MODE		"Fail to enter into program mode, try slower frequency?"
#define ERRCODE_FAILURE_ENTER_PROG_MODE		VSFERR_FAIL

#define ERRMSG_INVALID_CHIP_ID				"Chip-id unmatch, read=0x%X, want=0x%X"
#define ERRCODE_INVALID_CHIP_ID				VSFERR_FAIL

#define ERRMSG_FAILURE_VERIFY_STR			"%s verify failed, read=%s, want=%s."
#define ERRMSG_FAILURE_VERIFY_AT_02X		"%s verify failed at 0x%X, read=0x%02X, want=0x%02X."
#define ERRCODE_FAILURE_VERIFY				VSFERR_FAIL

#define ERRMSG_FAILURE_OPERATION_ADDR		"Fail to %s at 0x%X."
#define ERRCODE_FAILURE_OPERATION_ADDR		VSFERR_FAIL

// User defined information messages
#define INFOMSG_BOOTLOADER_VERSION			"Bootloader version %d.%d"
#define INFOMSG_REG_02X						"%s = 0x%02X"
#define INFOMSG_REG_04X						"%s = 0x%04X"
#define INFOMSG_REG_06X						"%s = 0x%06X"
#define INFOMSG_REG_08X						"%s = 0x%08X"
#define	INFOMSG_REG_08X_STR					INFOMSG_REG_08X ", %s"
#define INFOMSG_TRY_AUTODETECT				"Chip name undefined, try auto-detecting."
#define INFOMSG_AUTODETECT_SIGNATURE		"Auto-detect signature is 0x%X"
#define INFOMSG_CHIP_FOUND					"%s found"
#define INFOMSG_TARGET_VOLTAGE				"Target runs at %.3fV"
#define INFOMSG_VOLTAGE						"%s = %.3fV"
#define INFOMSG_TARGET_LOW_POWER			"No power is detected on target."
#define INFOMSG_USE_DEFAULT					"%s not defined, use %s as default."
#define INFOMSG_TARGET_CHIP_ID				"Chip-id read is 0x%X."
#define INFOMSG_TARGET_READ					"%s read is %s"
#define INFOMSG_CHECKSUM					"Checksum is 0x%X."
#define INFOMSG_CHECKSUM_BANK				"Checksum at band%d is 0x%X."

#define INFOMSG_CHECKING					"checking %s"
#define INFOMSG_CHECKED						"%s checked"
#define INFOMSG_ERASING						"erasing %s"
#define INFOMSG_ERASED						"%s erased"
#define INFOMSG_PROGRAMMING					"programming %s"
#define INFOMSG_PROGRAMMED_SIZE				"%s programmed for %dbytes(%.2fKB/s)"
#define INFOMSG_PROGRAMMED					"%s programmed"
#define INFOMSG_VERIFYING					"verifying %s"
#define INFOMSG_VERIFIED_SIZE				"%s verified for %dbytes(%.2fKB/s)"
#define INFOMSG_VERIFIED					"%s verified"
#define INFOMSG_READING						"reading %s"
#define INFOMSG_READ_SIZE					"%s read for %dbytes(%.2fKB/s)"
#define INFOMSG_READ						"%s read"

#endif /* __APP_ERR_H_INCLUDED__ */

