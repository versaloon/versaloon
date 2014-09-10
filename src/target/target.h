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
#ifndef __TARGET_H_INCLUDED__
#define __TARGET_H_INCLUDED__

#include "tool/list/list.h"

#define ASSEMBLE_FUNCNAME(mod, name)	mod ## name

// Target Feature
#define AUTO_DETECT					"A"
#define USE_COMM					"C"
#define CAN_EXECUTE					"X"
#define SET_FREQUENCY				"F"
#define NO_TARGET					"N"

// Target Area
#define CHIPID_IDX					0
#define CHIPID_CHKSUM_IDX			(1 + CHIPID_IDX)
#define BOOTLOADER_IDX				2
#define BOOTLOADER_CHKSUM_IDX		(1 + BOOTLOADER_IDX)
#define APPLICATION_IDX				4
#define APPLICATION_CHKSUM_IDX		(1 + APPLICATION_IDX)
#define	EEPROM_IDX					6
#define EEPROM_CHKSUM_IDX			(1 + EEPROM_IDX)
#define	OTPROM_IDX					8
#define OTPROM_CHKSUM_IDX			(1 + OTPROM_IDX)
#define FUSE_IDX					10
#define FUSE_CHKSUM_IDX				(1 + FUSE_IDX)
#define LOCK_IDX					12
#define LOCK_CHKSUM_IDX				(1 + LOCK_IDX)
#define USRSIG_IDX					14
#define USRSIG_CHKSUM_IDX			(1 + USRSIG_IDX)
#define CALIBRATION_IDX				16
#define CALIBRATION_CHKSUM_IDX		(1 + CALIBRATION_IDX)
#define RAM_IDX						18
#define SPECIAL_STRING_IDX			19
#define UNIQUEID_IDX				20

#define CHIPID						(1 << CHIPID_IDX)
#define CHIPID_CHKSUM				(1 << CHIPID_CHKSUM_IDX)
#define BOOTLOADER					(1 << BOOTLOADER_IDX)
#define BOOTLOADER_CHKSUM			(1 << BOOTLOADER_CHKSUM_IDX)
#define APPLICATION					(1 << APPLICATION_IDX)
#define APPLICATION_CHKSUM			(1 << APPLICATION_CHKSUM_IDX)
#define EEPROM						(1 << EEPROM_IDX)
#define EEPROM_CHKSUM				(1 << EEPROM_CHKSUM_IDX)
#define OTPROM						(1 << OTPROM_IDX)
#define OTPROM_CHKSUM				(1 << OTPROM_CHKSUM_IDX)
#define FUSE						(1 << FUSE_IDX)
#define FUSE_CHKSUM					(1 << FUSE_CHKSUM_IDX)
#define LOCK						(1 << LOCK_IDX)
#define LOCK_CHKSUM					(1 << LOCK_CHKSUM_IDX)
#define USRSIG						(1 << USRSIG_IDX)
#define USRSIG_CHKSUM				(1 << USRSIG_CHKSUM_IDX)
#define CALIBRATION					(1 << CALIBRATION_IDX)
#define CALIBRATION_CHKSUM			(1 << CALIBRATION_CHKSUM_IDX)
#define RAM							(1 << RAM_IDX)
#define SPECIAL_STRING				(1 << SPECIAL_STRING_IDX)
#define UNIQUEID					(1 << UNIQUEID_IDX)
#define ALL							0x80000000UL
#define TARGET_AREA_MASK			(BOOTLOADER | APPLICATION | EEPROM \
									 | OTP_ROM | FUSE | LOCK | USER_SIG \
									 | CHECKSUM | ALL)
#define NUM_OF_TARGET_AREA			21

enum area_attr_t
{
	AREA_ATTR_NONE	= 0,
	AREA_ATTR_E		= (1 << 0), // Erasable
	AREA_ATTR_W		= (1 << 1), // Writable
	AREA_ATTR_R		= (1 << 2), // Readable
	AREA_ATTR_EP	= (1 << 3), // Erase in PageMode
	AREA_ATTR_WNP	= (1 << 4), // Non-Paged Mode when write
	AREA_ATTR_RNP	= (1 << 5), // Non-Paged Mode when read
	AREA_ATTR_V		= (1 << 6), // Simple verify, eg: checksum
	AREA_ATTR_RAE	= (1 << 7), // Reset After Erase
	AREA_ATTR_RAW	= (1 << 8), // Reset After Write
	AREA_ATTR_EWW	= (1 << 9), // Erase While Write
	AREA_ATTR_WR	= AREA_ATTR_R | AREA_ATTR_W,
	AREA_ATTR_ER	= AREA_ATTR_R | AREA_ATTR_E,
	AREA_ATTR_EW	= AREA_ATTR_E | AREA_ATTR_W,
	AREA_ATTR_EWR	= AREA_ATTR_R | AREA_ATTR_W | AREA_ATTR_E,
	AREA_ATTR_NP	= AREA_ATTR_WNP | AREA_ATTR_RNP
};

#define ALL_CHAR					'*'
#define CHIPID_CHAR					'i'
#define CHIPID_CHKSUM_CHAR			'I'
#define BOOTLOADER_CHAR				'b'
#define BOOTLOADER_CHKSUM_CHAR		'B'
#define	APPLICATION_CHAR			'f'
#define APPLICATION_CHKSUM_CHAR		'F'
#define EEPROM_CHAR					'e'
#define EEPROM_CHKSUM_CHAR			'E'
#define OTPROM_CHAR					'o'
#define OTPROM_CHKSUM_CHAR			'O'
#define FUSE_CHAR					'u'
#define FUSE_CHKSUM_CHAR			'U'
#define LOCK_CHAR					'l'
#define LOCK_CHKSUM_CHAR			'L'
#define USRSIG_CHAR					's'
#define USRSIG_CHKSUM_CHAR			'S'
#define CALIBRATION_CHAR			'c'
#define CALIBRATION_CHKSUM_CHAR		'C'
#define RAM_CHAR					'r'
#define SPECIAL_STRING_CHAR			't'
#define UNIQUEID_CHAR				'q'

struct target_area_name_t
{
	const char name;
	uint32_t mask;
	const char *full_name;
};

extern const struct target_area_name_t target_area_name[NUM_OF_TARGET_AREA];

struct program_area_t
{
	char *cli_str;
	uint8_t *buff;
	uint32_t size;
	struct memlist *memlist;
	struct memlist *exact_memlist;
};

struct program_info_t
{
	char *chip_type;
	char *chip_name;
	uint32_t chip_id;
	uint32_t chip_address;
	
	struct program_area_t program_areas[dimof(target_area_name)];
	uint32_t areas_defined;
	
	uint8_t mode;
	char mode_char;
	
	uint8_t auto_adjust;
	uint16_t frequency;
	uint32_t kernel_khz;
	uint32_t quartz_khz;
	bool erase_on_demand;
	uint8_t wait_state;
	struct jtag_pos_t jtag_pos;
	uint8_t execute_flag;
	uint32_t execute_addr;
	bool raw;
	char *ifs_indexes;
	char *param;
	
	uint8_t *mass_product_data;
	uint32_t mass_product_data_size;
};

struct program_area_map_t
{
	char name;
	uint8_t data_pos;
	int32_t fseg_addr;
	int32_t fstart_addr;
	uint16_t fpage_size;
	enum area_attr_t attr;
};

struct program_mode_t
{
	char name;
	const char *feature;
	uint64_t interface_needed;
};

struct chip_area_info_t
{
	uint32_t index;
	uint32_t seg;
	uint32_t addr;
	uint32_t page_size;
	uint32_t page_num;
	uint64_t default_value;
	uint32_t size;
	uint8_t *mask;
	char *cli_format;
	
	struct sllist list;
};

struct chip_param_t
{
	char *chip_name;
	uint32_t chip_id;
	char *program_mode_str;
	uint32_t program_mode;
	
	uint32_t chip_erase;
	uint32_t param[32];
	
	struct chip_area_info_t *chip_areas;
};

#define	EXECUTE_FUNCNAME(mod)				ASSEMBLE_FUNCNAME(mod, _execute)
#define ENTER_PROGRAM_MODE_FUNCNAME(mod)	ASSEMBLE_FUNCNAME(mod, _enter_program_mode)
#define LEAVE_PROGRAM_MODE_FUNCNAME(mod)	ASSEMBLE_FUNCNAME(mod, _leave_program_mode)
#define ERASE_TARGET_FUNCNAME(mod)			ASSEMBLE_FUNCNAME(mod, _erase_target)
#define WRITE_TARGET_FUNCNAME(mod)			ASSEMBLE_FUNCNAME(mod, _write_target)
#define READ_TARGET_FUNCNAME(mod)			ASSEMBLE_FUNCNAME(mod, _read_target)
#define SWITCH_TARGET_FUNCNAME(mod)			ASSEMBLE_FUNCNAME(mod, _switch_target)

#define EXECUTE_HANDLER(mod)				\
			static vsf_err_t EXECUTE_FUNCNAME(mod)\
						(struct program_context_t *context)
#define ENTER_PROGRAM_MODE_HANDLER(mod)		\
			static vsf_err_t ENTER_PROGRAM_MODE_FUNCNAME(mod)\
						(struct program_context_t *context)
#define LEAVE_PROGRAM_MODE_HANDLER(mod)		\
			static vsf_err_t LEAVE_PROGRAM_MODE_FUNCNAME(mod)\
						(struct program_context_t *context, uint8_t success)
#define ERASE_TARGET_HANDLER(mod)			\
			static vsf_err_t ERASE_TARGET_FUNCNAME(mod)\
						(struct program_context_t *context, char area, \
							uint32_t addr, uint32_t size)
#define WRITE_TARGET_HANDLER(mod)			\
			static vsf_err_t WRITE_TARGET_FUNCNAME(mod)\
						(struct program_context_t *context, char area, \
							uint32_t addr, uint8_t *buff, uint32_t size)
#define READ_TARGET_HANDLER(mod)			\
			static vsf_err_t READ_TARGET_FUNCNAME(mod)\
						(struct program_context_t *context, char area, \
							uint32_t addr, uint8_t *buff, uint32_t size)
#define SWITCH_TARGET_HANDLER(mod)			\
			static vsf_err_t SWITCH_TARGET_FUNCNAME(mod)\
						(struct program_context_t *context)

#define	PARSE_ARGUMENT_FUNCNAME(mod)		ASSEMBLE_FUNCNAME(mod, _parse_argument)
#define ADJUST_SETTING_FUNCNAME(mod)		ASSEMBLE_FUNCNAME(mod, _adjust_setting)
#define ADJUST_SETTING_HANDLER(mod)			\
			vsf_err_t ADJUST_SETTING_FUNCNAME(mod)\
					(struct program_info_t *pi, struct chip_param_t *param, \
						uint32_t program_mode)

#define ADJUST_MAPPING_FUNCNAME(mod)		ASSEMBLE_FUNCNAME(mod, _adjust_mapping)
#define ADJUST_MAPPING_HANDLER(mod)			\
			vsf_err_t ADJUST_MAPPING_FUNCNAME(mod)\
					(uint32_t *address, uint8_t dir)

#define TARGET_MAPPING_FROM_FILE			1
#define TARGET_MAPPING_TO_FILE				0

struct target_info_t
{
	const char *name;
	const char *feature;
	const struct program_area_map_t *program_area_map;
	const struct program_mode_t *program_mode;
	const struct program_functions_t *program_functions;
	const struct vss_cmd_t *notifier;
	vsf_err_t (*adjust_setting)(struct program_info_t *pi,
							struct chip_param_t *param, uint32_t program_mode);
	vsf_err_t (*adjust_mapping)(uint32_t *address, uint8_t dir);
};

struct program_context_t
{
	struct operation_t *op;
	struct target_info_t *target;
	struct program_info_t *pi;
	struct chip_param_t *param;
	struct INTERFACES_INFO_T *prog;
	struct chip_series_t *series;
	
	// private data
	// use dedicated buffer to avoid static data or malloc in target code
	uint8_t priv[256];
};

struct program_functions_t
{
	vsf_err_t (*execute)(struct program_context_t *context);
	vsf_err_t (*enter_program_mode)(struct program_context_t *context);
	vsf_err_t (*leave_program_mode)(struct program_context_t *context,
									uint8_t success);
	// erase one page at addr or erase full target
	vsf_err_t (*erase_target)(struct program_context_t *context, char area,
								uint32_t addr, uint32_t size);
	// write one page at addr
	vsf_err_t (*write_target)(struct program_context_t *context, char area,
								uint32_t addr, uint8_t *buff, uint32_t size);
	// read one page at addr
	vsf_err_t (*read_target)(struct program_context_t *context, char area,
								uint32_t addr, uint8_t *buff, uint32_t size);
	// verify one page at addr
//	vsf_err_t (*verify_target)(struct program_context_t *context, char area,
//								uint32_t addr, uint8_t *buff, uint32_t size);
	// switch targets
	vsf_err_t (*switch_target)(struct program_context_t *context);
};

struct chip_series_t
{
	char *series_name;
	uint32_t size;
	uint32_t num_of_chips;
	struct chip_param_t *chips_param;
};

struct chip_fl_warning_t
{
	char *mask;
	char *value;
	char *msg;
	uint8_t ban;
};

struct chip_fl_choice_t
{
	char *value;
	char *text;
};

struct chip_fl_setting_t
{
	char *name;
	char *info;
	char *mask;
	char *ban;
	char *format;
	uint8_t use_checkbox;
	uint8_t use_edit;
	uint8_t shift;
	uint8_t radix;
	char *checked;
	char *unchecked;
	uint8_t bytelen;
	uint16_t num_of_choices;
	struct chip_fl_choice_t *choices;
};

struct chip_fl_t
{
	char * init_value;
	uint16_t num_of_fl_warnings;
	struct chip_fl_warning_t *warnings;
	uint16_t num_of_fl_settings;
	struct chip_fl_setting_t *settings;
};

extern struct vss_cmd_list_t target_cmd_list;

extern const struct target_info_t targets_info[];

uint32_t target_area_mask(char area_name);
char* target_area_fullname(char area_name);
int8_t target_area_idx(char area_name);
char target_area_char_by_fullname(char *fullname);
char* target_area_fullname_by_mask(uint32_t mask);

int8_t target_mode_get_idx(const struct program_mode_t *mode, char mode_name);

vsf_err_t target_prepare_operations(struct program_context_t *context,
									uint32_t *readfile, uint32_t *writefile);

void target_chip_area_free(struct chip_area_info_t *area_info);
struct chip_area_info_t* target_chip_area_dup(
										struct chip_area_info_t *area_info);
struct chip_area_info_t* target_get_chip_area(struct chip_param_t *param,
												uint32_t area_idx);
struct program_area_t* target_get_program_area(struct program_info_t *pi,
												uint32_t area_idx);
void target_get_target_area(struct program_info_t *pi, char area,
							uint8_t **buff, uint32_t *size);

struct target_cfg_data_info_t
{
	uint32_t addr;
	bool little_endian;
	uint8_t addr_width;
	uint8_t align;
};
vsf_err_t target_generate_cfg_data(struct target_cfg_data_info_t *cfg_data_info,
									const char *filename);
vsf_err_t target_generate_data(struct target_cfg_data_info_t *cfg_data_info,
					struct program_context_t *context, const char *filename);

vsf_err_t target_release_chip_series(struct chip_series_t *s);
vsf_err_t target_release_chip_fl(struct chip_fl_t *fl);

void target_print_target(uint32_t index);
void target_print_list(void);
void target_print_help(void);

vsf_err_t target_data_free(struct program_context_t *context);
vsf_err_t target_data_read(struct program_context_t *context);
vsf_err_t target_data_save(struct program_context_t *context);

#endif /* __TARGET_H_INCLUDED__ */

