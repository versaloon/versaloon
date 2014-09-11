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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include <libxml/parser.h>

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"

#include "strparser.h"

#define TARGET_CONF_FILE_EXT			".xml"

static uint32_t target_xml_get_child_number(xmlNodePtr parentNode,
												char * child_name)
{
	uint32_t result = 0;
	
	if ((parentNode != NULL) && (parentNode->children != NULL))
	{
		parentNode = parentNode->children->next;
		while (parentNode != NULL)
		{
			if (!xmlStrcmp(parentNode->name, BAD_CAST child_name))
			{
				result++;
			}
			parentNode = parentNode->next->next;
		}
	}
	
	return result;
}

vsf_err_t target_release_chip_fl(struct chip_fl_t *fl)
{
	uint32_t i, j;
	
	if (NULL == fl)
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
	
	if (fl->init_value != NULL)
	{
		free(fl->init_value);
		fl->init_value = NULL;
	}
	
	// free warnings
	if (fl->warnings != NULL)
	{
		for (i = 0; i < fl->num_of_fl_warnings; i++)
		{
			if (fl->warnings[i].mask != NULL)
			{
				free(fl->warnings[i].mask);
				fl->warnings[i].mask = NULL;
			}
			if (fl->warnings[i].value != NULL)
			{
				free(fl->warnings[i].value);
				fl->warnings[i].value = NULL;
			}
			if (fl->warnings[i].msg != NULL)
			{
				free(fl->warnings[i].msg);
				fl->warnings[i].msg = NULL;
			}
		}
		free(fl->warnings);
		fl->warnings = NULL;
	}
	
	// free settings
	if (fl->settings != NULL)
	{
		for (i = 0; i < fl->num_of_fl_settings; i++)
		{
			if (fl->settings[i].name != NULL)
			{
				free(fl->settings[i].name);
				fl->settings[i].name = NULL;
			}
			if (fl->settings[i].ban != NULL)
			{
				free(fl->settings[i].ban);
				fl->settings[i].ban = NULL;
			}
			if (fl->settings[i].info != NULL)
			{
				free(fl->settings[i].info);
				fl->settings[i].info = NULL;
			}
			if (fl->settings[i].format != NULL)
			{
				free(fl->settings[i].format);
				fl->settings[i].format = NULL;
			}
			if (fl->settings[i].mask != NULL)
			{
				free(fl->settings[i].mask);
				fl->settings[i].mask = NULL;
			}
			if (fl->settings[i].checked != NULL)
			{
				free(fl->settings[i].checked);
				fl->settings[i].checked = NULL;
			}
			if (fl->settings[i].unchecked != NULL)
			{
				free(fl->settings[i].unchecked);
				fl->settings[i].unchecked = NULL;
			}
			if (fl->settings[i].choices != NULL)
			{
				for (j = 0; j < fl->settings[i].num_of_choices; j++)
				{
					if (fl->settings[i].choices[j].value != NULL)
					{
						free(fl->settings[i].choices[j].value);
						fl->settings[i].choices[j].value = NULL;
					}
					if (fl->settings[i].choices[j].text != NULL)
					{
						free(fl->settings[i].choices[j].text);
						fl->settings[i].choices[j].text = NULL;
					}
				}
				free(fl->settings[i].choices);
				fl->settings[i].choices = NULL;
			}
		}
		free(fl->settings);
		fl->settings = NULL;
	}
	memset(fl, 0, sizeof(struct chip_fl_t));
	
	return VSFERR_NONE;
}

vsf_err_t target_build_chip_fl(struct target_info_t *target,
				const char *chip_module, char *type, struct chip_fl_t *fl)
{
	xmlDocPtr doc = NULL;
	xmlNodePtr curNode = NULL;
	xmlNodePtr paramNode, settingNode;
	char *filename = NULL;
	uint32_t i, j, num_of_chips;
	vsf_err_t err = VSFERR_NONE;
	FILE *fp;
	char *format;
	char format_tmp[32];
	uint8_t size;
	
	if ((NULL == config_dir) || (NULL == target) || (NULL == target->name) ||
		(NULL == fl) || (NULL == chip_module))
	{
		return VSFERR_FAIL;
	}
	
	// release first if necessary
	target_release_chip_fl(fl);
	
	filename = (char *)malloc(strlen(config_dir)
					+ strlen(target->name) + strlen(TARGET_CONF_FILE_EXT) + 1);
	if (NULL == filename)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	strcpy(filename, config_dir);
	strcat(filename, target->name);
	strcat(filename, TARGET_CONF_FILE_EXT);
	fp = fopen(filename, "r");
	if (NULL == fp)
	{
		// no error message, just return error
		err = VSFERR_FAIL;
		goto free_and_exit;
	}
	else
	{
		fclose(fp);
		fp = NULL;
	}
	
	doc = xmlReadFile(filename, "", XML_PARSE_RECOVER);
	if (NULL == doc)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPEN, filename);
		err = ERRCODE_FAILURE_OPEN;
		goto free_and_exit;
	}
	curNode = xmlDocGetRootElement(doc);
	if (NULL == curNode)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		err = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	// valid check
	if (xmlStrcmp(curNode->name, BAD_CAST "series")
		|| !xmlHasProp(curNode, BAD_CAST "name")
		|| xmlStrcmp(xmlGetProp(curNode, BAD_CAST "name"),
					 (const xmlChar *)target->name))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		err = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	num_of_chips = target_xml_get_child_number(curNode, "chip");
	if (0 == num_of_chips)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, target->name);
		err = VSFERR_NOT_SUPPORT;
		goto free_and_exit;
	}
	
	// read data
	curNode = curNode->children->next;
	for (i = 0; i < num_of_chips; i++)
	{
		// check
		if ((NULL == curNode)
			|| xmlStrcmp(curNode->name, BAD_CAST "chip")
			|| !xmlHasProp(curNode, BAD_CAST "name"))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
			err = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		if (strcmp((const char *)chip_module,
				   (const char *)xmlGetProp(curNode, BAD_CAST "name")))
		{
			// not the chip I want
			curNode = curNode->next->next;
			continue;
		}
		else
		{
			break;
		}
	}
	if (i >= num_of_chips)
	{
		// not found
		goto free_and_exit;
	}
	
	paramNode = curNode->children->next;
	// read parameters
	while((paramNode != NULL) && xmlStrcmp(paramNode->name, BAD_CAST type))
	{
		paramNode = paramNode->next->next;
	}
	if (NULL == paramNode)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, type, chip_module);
		err = VSFERR_NOT_SUPPORT;
		goto free_and_exit;
	}
	
	// we found the parameter
	// valid check
	if (!xmlHasProp(paramNode, BAD_CAST "init")
		|| !xmlHasProp(paramNode, BAD_CAST "bytesize"))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		err = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	size = (uint8_t)strtoul((char*)xmlGetProp(paramNode, BAD_CAST "bytesize"), NULL, 0);
	format = (char *)xmlGetProp(paramNode, BAD_CAST "format");
	if (NULL == format)
	{
		if (size > 8)
		{
			LOG_ERROR(ERRMSG_NOT_DEFINED, "format node");
			err = VSFERR_FAIL;
			goto free_and_exit;
		}
		snprintf(format_tmp, sizeof(format_tmp), "%%%dx", size);
		format = format_tmp;
	}
	
	// read fl number
	fl->num_of_fl_settings =
		(uint16_t)target_xml_get_child_number(paramNode, "setting");
	if (0 == fl->num_of_fl_settings)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, type, chip_module);
		err = VSFERR_NOT_SUPPORT;
		goto free_and_exit;
	}
	
	// read fl init value
	fl->init_value = strdup((char *)xmlGetProp(paramNode, BAD_CAST "init"));
	if (NULL == fl->init_value)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		err = VSFERR_NOT_ENOUGH_RESOURCES;
		goto free_and_exit;
	}
	if (strparser_check(fl->init_value, format))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse init node");
		err = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	// alloc memory for settings
	fl->settings = (struct chip_fl_setting_t*)malloc(
		fl->num_of_fl_settings * sizeof(struct chip_fl_setting_t));
	if (NULL == fl->settings)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		err = VSFERR_NOT_ENOUGH_RESOURCES;
		goto free_and_exit;
	}
	memset(fl->settings, 0,
		   fl->num_of_fl_settings * sizeof(struct chip_fl_setting_t));
	
	settingNode = paramNode->children->next;
	// has warning?
	if ((settingNode != NULL) &&
		!strcmp((const char *)settingNode->name, "warning"))
	{
		xmlNodePtr warningNode = settingNode;
		xmlNodePtr wNode;
		
		settingNode = settingNode->next->next;
		// parse warning
		fl->num_of_fl_warnings =
			(uint16_t)target_xml_get_child_number(warningNode, "w");
		if (fl->num_of_fl_warnings != 0)
		{
			fl->warnings = (struct chip_fl_warning_t*)malloc(
				fl->num_of_fl_warnings * sizeof(struct chip_fl_warning_t));
			if (NULL == fl->warnings)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				err = VSFERR_NOT_ENOUGH_RESOURCES;
				goto free_and_exit;
			}
			memset(fl->warnings, 0,
				   fl->num_of_fl_warnings * sizeof(struct chip_fl_warning_t));
			
			wNode = warningNode->children->next;
			for (i = 0; i < fl->num_of_fl_warnings; i++)
			{
				// check
				if (strcmp((const char *)wNode->name, "w"))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				
				fl->warnings[i].mask = strdup(
									(char *)xmlGetProp(wNode, BAD_CAST "mask"));
				if (NULL == fl->warnings[i].mask)
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					err = VSFERR_NOT_ENOUGH_RESOURCES;
					goto free_and_exit;
				}
				if (strparser_check(fl->warnings[i].mask, format))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				
				fl->warnings[i].value = strdup(
									(char *)xmlGetProp(wNode, BAD_CAST "value"));
				if (NULL == fl->warnings[i].value)
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					err = VSFERR_NOT_ENOUGH_RESOURCES;
					goto free_and_exit;
				}
				if (strparser_check(fl->warnings[i].value, format))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse value node");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				
				fl->warnings[i].msg = strdup(
									(char *)xmlGetProp(wNode, BAD_CAST "msg"));
				if (NULL == fl->warnings[i].msg)
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					err = VSFERR_NOT_ENOUGH_RESOURCES;
					goto free_and_exit;
				}
				
				fl->warnings[i].ban = 0;
				if (xmlHasProp(wNode, BAD_CAST "ban"))
				{
					fl->warnings[i].ban = (uint8_t)strtoul(
						(const char *)xmlGetProp(wNode, BAD_CAST "ban"),
						NULL, 0);
				}
				
				wNode = wNode->next->next;
			}
		}
	}
	
	// parse settings
	for (i = 0; i < fl->num_of_fl_settings; i++)
	{
		xmlNodePtr choiceNode;
		
		fl->settings[i].num_of_choices =
			(uint16_t)target_xml_get_child_number(settingNode, "choice");
		// check
		if (strcmp((const char *)settingNode->name, "setting")
			|| (!xmlHasProp(settingNode, BAD_CAST "name"))
			|| (!xmlHasProp(settingNode, BAD_CAST "mask")))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
			err = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		fl->settings[i].name = strdup(
							(char *)xmlGetProp(settingNode, BAD_CAST "name"));
		if (NULL == fl->settings[i].name)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			err = VSFERR_NOT_ENOUGH_RESOURCES;
			goto free_and_exit;
		}
		
		fl->settings[i].mask = strdup(
							(char *)xmlGetProp(settingNode, BAD_CAST "mask"));
		if (NULL == fl->settings[i].mask)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			err = VSFERR_NOT_ENOUGH_RESOURCES;
			goto free_and_exit;
		}
		if (strparser_check(fl->settings[i].mask, format))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
			err = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		if (xmlHasProp(settingNode, BAD_CAST "ban"))
		{
			fl->settings[i].ban = strdup(
							(char *)xmlGetProp(settingNode, BAD_CAST "ban"));
			if (NULL == fl->settings[i].ban)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				err = VSFERR_NOT_ENOUGH_RESOURCES;
				goto free_and_exit;
			}
		}
		
		// parse info if exists
		if (xmlHasProp(settingNode, BAD_CAST "info"))
		{
			fl->settings[i].info = strdup(
							(char *)xmlGetProp(settingNode, BAD_CAST "info"));
			if (NULL == fl->settings[i].info)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				err = VSFERR_NOT_ENOUGH_RESOURCES;
				goto free_and_exit;
			}
		}
		
		// parse format if exists
		if (xmlHasProp(settingNode, BAD_CAST "format"))
		{
			fl->settings[i].format = strdup(
							(char *)xmlGetProp(settingNode, BAD_CAST "format"));
			if (NULL == fl->settings[i].format)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				err = VSFERR_NOT_ENOUGH_RESOURCES;
				goto free_and_exit;
			}
		}
		
		// parse bytelen if exists
		if (xmlHasProp(settingNode, BAD_CAST "bytelen"))
		{
			fl->settings[i].bytelen = (uint8_t)strtoul(
				(const char *)xmlGetProp(settingNode, BAD_CAST "bytelen"),
				NULL, 0);
		}
		
		// parse radix if exists
		if (xmlHasProp(settingNode, BAD_CAST "radix"))
		{
			fl->settings[i].radix = (uint8_t)strtoul(
				(const char *)xmlGetProp(settingNode, BAD_CAST "radix"),
				NULL, 0);
		}
		
		// parse shift if exists
		if (xmlHasProp(settingNode, BAD_CAST "shift"))
		{
			fl->settings[i].shift = (uint8_t)strtoul(
				(const char *)xmlGetProp(settingNode, BAD_CAST "shift"),
				NULL, 0);
		}
		
		// parse checked or unchecked
		if (xmlHasProp(settingNode, BAD_CAST "checked"))
		{
			fl->settings[i].checked = strdup(
						(char *)xmlGetProp(settingNode, BAD_CAST "checked"));
			if (NULL == fl->settings[i].checked)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				err = VSFERR_NOT_ENOUGH_RESOURCES;
				goto free_and_exit;
			}
			if (strparser_check(fl->settings[i].checked, format))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse checked node");
				err = ERRCODE_FAILURE_OPERATION;
				goto free_and_exit;
			}
			if (!xmlHasProp(settingNode, BAD_CAST "unchecked"))
			{
				uint64_t val_tmp, mask_tmp;
				
				if (size > 8)
				{
					LOG_ERROR(ERRMSG_NOT_DEFINED, "unchecked node");
					err = VSFERR_FAIL;
					goto free_and_exit;
				}
				val_tmp = 0;
				if (strparser_parse(fl->settings[i].checked,
							format, (uint8_t*)&val_tmp, sizeof(val_tmp)))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse checked node");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				mask_tmp = 0;
				if (strparser_parse(fl->settings[i].mask,
							format, (uint8_t*)&mask_tmp, sizeof(mask_tmp)))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				val_tmp ^= mask_tmp;
				fl->settings[i].unchecked =
					strparser_solve(format, (uint8_t*)&val_tmp,
									sizeof(val_tmp));
				if (NULL == fl->settings[i].unchecked)
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION,
								"solve unchecked value");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
			}
			fl->settings[i].use_checkbox = 1;
		}
		if (xmlHasProp(settingNode, BAD_CAST "unchecked"))
		{
			fl->settings[i].unchecked = strdup(
						(char *)xmlGetProp(settingNode, BAD_CAST "unchecked"));
			if (NULL == fl->settings[i].unchecked)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				err = VSFERR_NOT_ENOUGH_RESOURCES;
				goto free_and_exit;
			}
			if (strparser_check(fl->settings[i].unchecked, format))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse unchecked node");
				err = ERRCODE_FAILURE_OPERATION;
				goto free_and_exit;
			}
			if (!xmlHasProp(settingNode, BAD_CAST "checked"))
			{
				uint64_t val_tmp, mask_tmp;
				
				if (size > 8)
				{
					LOG_ERROR(ERRMSG_NOT_DEFINED, "checked node");
					err = VSFERR_FAIL;
					goto free_and_exit;
				}
				val_tmp = 0;
				if (strparser_parse(fl->settings[i].unchecked,
							format, (uint8_t*)&val_tmp, sizeof(val_tmp)))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse unchecked node");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				mask_tmp = 0;
				if (strparser_parse(fl->settings[i].mask,
							format, (uint8_t*)&mask_tmp, sizeof(mask_tmp)))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				val_tmp ^= mask_tmp;
				fl->settings[i].checked =
					strparser_solve(format, (uint8_t*)&val_tmp,
									sizeof(val_tmp));
				if (NULL == fl->settings[i].checked)
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "solve checked value");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
			}
			fl->settings[i].use_checkbox = 1;
		}
		
		if (!fl->settings[i].use_checkbox
			&& (0 == fl->settings[i].num_of_choices))
		{
			fl->settings[i].use_edit = 1;
		}
		else
		{
			fl->settings[i].use_edit = 0;
		}
		
		// parse choices
		if (0 == fl->settings[i].num_of_choices)
		{
			// no choice
			settingNode = settingNode->next->next;
			continue;
		}
		
		// malloc memory for choices
		fl->settings[i].choices = (struct chip_fl_choice_t*)malloc(
			fl->settings[i].num_of_choices * sizeof(struct chip_fl_choice_t));
		if (NULL == fl->settings[i].choices)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			err = VSFERR_NOT_ENOUGH_RESOURCES;
			goto free_and_exit;
		}
		memset(fl->settings[i].choices, 0,
			fl->settings[i].num_of_choices * sizeof(struct chip_fl_choice_t));
		
		choiceNode = settingNode->children->next;
		// parse choices
		for (j = 0; j < fl->settings[i].num_of_choices; j++)
		{
			// check
			if (strcmp((const char *)choiceNode->name, "choice")
				|| !xmlHasProp(choiceNode, BAD_CAST "value")
				|| !xmlHasProp(choiceNode, BAD_CAST "text"))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
				err = ERRCODE_FAILURE_OPERATION;
				goto free_and_exit;
			}
			
			// parse
			fl->settings[i].choices[j].value = strdup(
							(char *)xmlGetProp(choiceNode, BAD_CAST "value"));
			if (NULL == fl->settings[i].choices[j].value)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				err = VSFERR_NOT_ENOUGH_RESOURCES;
				goto free_and_exit;
			}
			if (strparser_check(fl->settings[i].choices[j].value,
												format))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse value node");
				err = ERRCODE_FAILURE_OPERATION;
				goto free_and_exit;
			}
			
			fl->settings[i].choices[j].text = strdup(
							(char *)xmlGetProp(choiceNode, BAD_CAST "text"));
			if (NULL == fl->settings[i].choices[j].text)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				err = VSFERR_NOT_ENOUGH_RESOURCES;
				goto free_and_exit;
			}
			
			choiceNode = choiceNode->next->next;
		}
		settingNode = settingNode->next->next;
	}
	
free_and_exit:
	if (filename != NULL)
	{
		free(filename);
		filename = NULL;
	}
	if (doc != NULL)
	{
		xmlFreeDoc(doc);
		doc = NULL;
	}
	
	return err;
}

static struct chip_area_info_t* target_assert_chip_area(
								struct chip_param_t *param, uint32_t area_idx)
{
	struct chip_area_info_t *area_info = NULL, *area_info_last = NULL, *temp;
	
	temp = param->chip_areas;
	while (temp != NULL)
	{
		area_info_last = temp;
		if (temp->index == area_idx)
		{
			area_info = temp;
			break;
		}
		temp = sllist_get_container(temp->list.next, struct chip_area_info_t,
									list);
	}
	
	if (NULL == area_info)
	{
		temp = (struct chip_area_info_t *)malloc(sizeof(struct chip_area_info_t));
		if (NULL == temp)
		{
			return NULL;
		}
		memset(temp, 0, sizeof(*temp));
		temp->index = area_idx;
		sllist_init_node(temp->list);
		
		if (NULL == area_info_last)
		{
			param->chip_areas = temp;
		}
		else
		{
			sllist_insert(area_info_last->list, temp->list);
		}
	}
	
	return area_info;
}

vsf_err_t target_release_chip_series(struct chip_series_t *s)
{
	struct chip_area_info_t *area_info = NULL, *temp = NULL;
	uint32_t i;
	
	if (s->series_name != NULL)
	{
		free(s->series_name);
		s->series_name = NULL;
	}
	
	if ((s != NULL) && ((s->num_of_chips > 0) || (s->chips_param != NULL)))
	{
		for (i = 0; i < s->num_of_chips; i++)
		{
			if (s->chips_param[i].chip_name != NULL)
			{
				free(s->chips_param[i].chip_name);
				s->chips_param[i].chip_name = NULL;
			}
			if (s->chips_param[i].program_mode_str != NULL)
			{
				free(s->chips_param[i].program_mode_str);
				s->chips_param[i].program_mode_str = NULL;
			}
			
			area_info = s->chips_param[i].chip_areas;
			while (area_info != NULL)
			{
				temp = area_info;
				if (area_info->mask != NULL)
				{
					free(area_info->mask);
					area_info->mask = NULL;
				}
				if (area_info->cli_format != NULL)
				{
					free(area_info->cli_format);
					area_info->cli_format = NULL;
				}
				area_info = sllist_get_container(area_info->list.next,
												struct chip_area_info_t, list);
				free(temp);
			}
		}
		free(s->chips_param);
		s->chips_param = NULL;
		s->num_of_chips = 0;
	}
	memset(s, 0, sizeof(struct chip_series_t));
	
	return VSFERR_NONE;
}

static vsf_err_t target_parse_chip_area(struct chip_area_info_t *area,
										xmlNodePtr node)
{
	uint32_t size;
	char *format, *str, format_tmp[32];
	
	area->size = (uint32_t)strtoul(
					(const char *)xmlGetProp(node, BAD_CAST "bytesize"),
					NULL, 0);
	size = area->size;
	area->default_value = strtoull(
					(const char *)xmlGetProp(node, BAD_CAST "init"),
					NULL, 0);
	area->cli_format = NULL;
	area->mask = NULL;
	str = (char *)xmlGetProp(node, BAD_CAST "format");
	if (str != NULL)
	{
		area->cli_format = strdup(str);
		if (NULL == area->cli_format)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		format = area->cli_format;
	}
	else
	{
		if (size > 8)
		{
			LOG_ERROR(ERRMSG_NOT_DEFINED, "format node");
			return VSFERR_FAIL;
		}
		snprintf(format_tmp, sizeof(format_tmp), "%%%dx", size);
		format = format_tmp;
	}
	str = (char *)xmlGetProp(node, BAD_CAST "mask");
	if (str != NULL)
	{
		area->mask = (uint8_t *)malloc(size);
		if (NULL == area->mask)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		if (strparser_parse(str, format, area->mask, size))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	return VSFERR_NONE;
}

vsf_err_t target_build_chip_series(struct target_info_t *target,
		const struct program_mode_t *program_mode, struct chip_series_t *s)
{
	xmlDocPtr doc = NULL;
	xmlNodePtr curNode = NULL;
	char *filename = NULL;
	struct chip_param_t *p_param;
	struct chip_area_info_t *p_area_info;
	uint32_t i, j, target_para_size_defined;
	vsf_err_t err = VSFERR_NONE;
	FILE *fp;
	
	if ((NULL == config_dir) || (NULL == target) || (NULL == target->name) ||
		(NULL == target->program_area_map) || (NULL == s))
	{
		return VSFERR_FAIL;
	}
	
	// release first if necessary
	target_release_chip_series(s);
	
	filename = (char *)malloc(strlen(config_dir)+ strlen(target->name)
								+ strlen(TARGET_CONF_FILE_EXT) + 1);
	if (NULL == filename)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	strcpy(filename, config_dir);
	strcat(filename, target->name);
	strcat(filename, TARGET_CONF_FILE_EXT);
	fp = fopen(filename, "r");
	if (NULL == fp)
	{
		// no error message, just return error
		err = VSFERR_FAIL;
		goto free_and_exit;
	}
	else
	{
		fclose(fp);
		fp = NULL;
	}
	
	doc = xmlReadFile(filename, "", XML_PARSE_RECOVER);
	if (NULL == doc)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPEN, filename);
		err = ERRCODE_FAILURE_OPEN;
		goto free_and_exit;
	}
	curNode = xmlDocGetRootElement(doc);
	if (NULL == curNode)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		err = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	// valid check
	if (xmlStrcmp(curNode->name, BAD_CAST "series")
		|| !xmlHasProp(curNode, BAD_CAST "name")
		|| xmlStrcmp(xmlGetProp(curNode, BAD_CAST "name"),
					 (const xmlChar *)target->name))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		err = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	s->series_name = strdup(target->name);
	if (NULL == s->series_name)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		err = VSFERR_NOT_ENOUGH_RESOURCES;
		goto free_and_exit;
	}
	s->num_of_chips = target_xml_get_child_number(curNode, "chip");
	if (0 == s->num_of_chips)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, target->name);
		err = VSFERR_NOT_SUPPORT;
		goto free_and_exit;
	}
	s->chips_param = (struct chip_param_t *)malloc(sizeof(struct chip_param_t)
											* s->num_of_chips);
	if (NULL == s->chips_param)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		err = VSFERR_NOT_ENOUGH_RESOURCES;
		goto free_and_exit;
	}
	memset(s->chips_param, 0, sizeof(struct chip_param_t) * s->num_of_chips);
	
#if 0
	// generate chip_areas according to program_area_map first
	{
		struct program_area_map_t *p_map;
		int8_t area_idx;
		
		for (i = 0; i < s->num_of_chips; i++)
		{
			p_param = &(s->chips_param[i]);
			p_map = (struct program_area_map_t *)target->program_area_map;
			
			while (p_map->name != 0)
			{
				area_idx = target_area_idx(p_map->name);
				if (area_idx < 0)
				{
					continue;
				}
				
				target_assert_chip_area(p_param, (uint32_t)area_idx);
				
				p_map++;
			}
		}
	}
#endif
	
	// read data
	curNode = curNode->children->next;
	for (i = 0; i < s->num_of_chips; i++)
	{
		xmlNodePtr paramNode;
		
		p_param = &(s->chips_param[i]);
		
		// check
		if ((NULL == curNode)
			|| xmlStrcmp(curNode->name, BAD_CAST "chip")
			|| !xmlHasProp(curNode, BAD_CAST "name"))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
			err = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		// read name
		p_param->chip_name = 
					strdup((const char *)xmlGetProp(curNode, BAD_CAST "name"));
		if (NULL == p_param->chip_name)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			err = VSFERR_NOT_ENOUGH_RESOURCES;
			goto free_and_exit;
		}
		
		// read parameters
		target_para_size_defined = 0;
		paramNode = curNode->children->next;
		while(paramNode != NULL)
		{
			if (!xmlStrcmp(paramNode->name, BAD_CAST "chip_id"))
			{
				p_param->chip_id = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "chip_erase"))
			{
				p_param->chip_erase = (uint8_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "program_mode"))
			{
				char *mode_tmp = (char *)xmlNodeGetContent(paramNode);
				int8_t mode_idx;
				
				p_param->program_mode_str = strdup(mode_tmp);
				if (NULL == p_param->program_mode_str)
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					err = VSFERR_NOT_ENOUGH_RESOURCES;
					goto free_and_exit;
				}
				p_param->program_mode = 0;
				if ((0 != i)
					|| (strcmp(target->name, p_param->chip_name)))
				{
					for (j = 0; j < strlen(mode_tmp); j++)
					{
						mode_idx =
								target_mode_get_idx(program_mode, mode_tmp[j]);
						if (mode_idx >= 0)
						{
							p_param->program_mode |= 1 << mode_idx;
						}
						else
						{
							LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, mode_tmp,
										"current target");
							err = VSFERR_NOT_SUPPORT;
							goto free_and_exit;
						}
					}
				}
				else
				{
					j = 0;
					while (program_mode[j].name != 0)
					{
						p_param->program_mode |= 1 << j;
						j++;
					}
				}
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_addr"))
			{
				target_assert_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info = target_get_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info->addr = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_seg"))
			{
				target_assert_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info = target_get_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info->seg = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_page_size"))
			{
				target_assert_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info = target_get_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info->page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_page_num"))
			{
				target_assert_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info = target_get_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info->page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_default"))
			{
				target_assert_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info = target_get_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info->default_value = (uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_addr"))
			{
				target_assert_chip_area(p_param, RAM_IDX);
				p_area_info = target_get_chip_area(p_param, RAM_IDX);
				p_area_info->addr = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_page_size"))
			{
				target_assert_chip_area(p_param, RAM_IDX);
				p_area_info = target_get_chip_area(p_param, RAM_IDX);
				p_area_info->page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_page_num"))
			{
				target_assert_chip_area(p_param, RAM_IDX);
				p_area_info = target_get_chip_area(p_param, RAM_IDX);
				p_area_info->page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "uid_addr"))
			{
				target_assert_chip_area(p_param, UNIQUEID_IDX);
				p_area_info = target_get_chip_area(p_param, UNIQUEID_IDX);
				p_area_info->addr = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "uid_page_size"))
			{
				target_assert_chip_area(p_param, UNIQUEID_IDX);
				p_area_info = target_get_chip_area(p_param, UNIQUEID_IDX);
				p_area_info->page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "uid_page_num"))
			{
				target_assert_chip_area(p_param, UNIQUEID_IDX);
				p_area_info = target_get_chip_area(p_param, UNIQUEID_IDX);
				p_area_info->page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_addr"))
			{
				target_assert_chip_area(p_param, APPLICATION_IDX);
				p_area_info = target_get_chip_area(p_param, APPLICATION_IDX);
				p_area_info->addr = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_seg"))
			{
				target_assert_chip_area(p_param, APPLICATION_IDX);
				p_area_info = target_get_chip_area(p_param, APPLICATION_IDX);
				p_area_info->seg = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_page_size"))
			{
				target_assert_chip_area(p_param, APPLICATION_IDX);
				p_area_info = target_get_chip_area(p_param, APPLICATION_IDX);
				p_area_info->page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_page_num"))
			{
				target_assert_chip_area(p_param, APPLICATION_IDX);
				p_area_info = target_get_chip_area(p_param, APPLICATION_IDX);
				p_area_info->page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_default"))
			{
				target_assert_chip_area(p_param, APPLICATION_IDX);
				p_area_info = target_get_chip_area(p_param, APPLICATION_IDX);
				p_area_info->default_value = (uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_addr"))
			{
				target_assert_chip_area(p_param, EEPROM_IDX);
				p_area_info = target_get_chip_area(p_param, EEPROM_IDX);
				p_area_info->addr = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_seg"))
			{
				target_assert_chip_area(p_param, EEPROM_IDX);
				p_area_info = target_get_chip_area(p_param, EEPROM_IDX);
				p_area_info->seg = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_page_size"))
			{
				target_assert_chip_area(p_param, EEPROM_IDX);
				p_area_info = target_get_chip_area(p_param, EEPROM_IDX);
				p_area_info->page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_page_num"))
			{
				target_assert_chip_area(p_param, EEPROM_IDX);
				p_area_info = target_get_chip_area(p_param, EEPROM_IDX);
				p_area_info->page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_default"))
			{
				target_assert_chip_area(p_param, EEPROM_IDX);
				p_area_info = target_get_chip_area(p_param, EEPROM_IDX);
				p_area_info->default_value = (uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_addr"))
			{
				target_assert_chip_area(p_param, OTPROM_IDX);
				p_area_info = target_get_chip_area(p_param, OTPROM_IDX);
				p_area_info->addr = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_seg"))
			{
				target_assert_chip_area(p_param, OTPROM_IDX);
				p_area_info = target_get_chip_area(p_param, OTPROM_IDX);
				p_area_info->seg = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_page_size"))
			{
				target_assert_chip_area(p_param, OTPROM_IDX);
				p_area_info = target_get_chip_area(p_param, OTPROM_IDX);
				p_area_info->page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_page_num"))
			{
				target_assert_chip_area(p_param, OTPROM_IDX);
				p_area_info = target_get_chip_area(p_param, OTPROM_IDX);
				p_area_info->page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_default"))
			{
				target_assert_chip_area(p_param, OTPROM_IDX);
				p_area_info = target_get_chip_area(p_param, OTPROM_IDX);
				p_area_info->default_value = (uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_addr"))
			{
				target_assert_chip_area(p_param, USRSIG_IDX);
				p_area_info = target_get_chip_area(p_param, USRSIG_IDX);
				p_area_info->addr = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_seg"))
			{
				target_assert_chip_area(p_param, USRSIG_IDX);
				p_area_info = target_get_chip_area(p_param, USRSIG_IDX);
				p_area_info->seg = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_page_size"))
			{
				target_assert_chip_area(p_param, USRSIG_IDX);
				p_area_info = target_get_chip_area(p_param, USRSIG_IDX);
				p_area_info->page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_page_num"))
			{
				target_assert_chip_area(p_param, USRSIG_IDX);
				p_area_info = target_get_chip_area(p_param, USRSIG_IDX);
				p_area_info->page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_default"))
			{
				target_assert_chip_area(p_param, USRSIG_IDX);
				p_area_info = target_get_chip_area(p_param, USRSIG_IDX);
				p_area_info->default_value = (uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_addr"))
			{
				target_assert_chip_area(p_param, FUSE_IDX);
				p_area_info = target_get_chip_area(p_param, FUSE_IDX);
				p_area_info->addr = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_seg"))
			{
				target_assert_chip_area(p_param, FUSE_IDX);
				p_area_info = target_get_chip_area(p_param, FUSE_IDX);
				p_area_info->seg = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_page_num"))
			{
				target_assert_chip_area(p_param, FUSE_IDX);
				p_area_info = target_get_chip_area(p_param, FUSE_IDX);
				p_area_info->page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_page_size"))
			{
				target_assert_chip_area(p_param, FUSE_IDX);
				p_area_info = target_get_chip_area(p_param, FUSE_IDX);
				p_area_info->page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_default"))
			{
				target_assert_chip_area(p_param, FUSE_IDX);
				p_area_info = target_get_chip_area(p_param, FUSE_IDX);
				p_area_info->default_value = (uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_addr"))
			{
				target_assert_chip_area(p_param, LOCK_IDX);
				p_area_info = target_get_chip_area(p_param, LOCK_IDX);
				p_area_info->addr = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_seg"))
			{
				target_assert_chip_area(p_param, LOCK_IDX);
				p_area_info = target_get_chip_area(p_param, LOCK_IDX);
				p_area_info->seg = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_page_num"))
			{
				target_assert_chip_area(p_param, LOCK_IDX);
				p_area_info = target_get_chip_area(p_param, LOCK_IDX);
				p_area_info->page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_page_size"))
			{
				target_assert_chip_area(p_param, LOCK_IDX);
				p_area_info = target_get_chip_area(p_param, LOCK_IDX);
				p_area_info->page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_area_info->size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_default"))
			{
				target_assert_chip_area(p_param, LOCK_IDX);
				p_area_info = target_get_chip_area(p_param, LOCK_IDX);
				p_area_info->default_value = (uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_size"))
			{
				target_para_size_defined |= FUSE;
				target_assert_chip_area(p_param, FUSE_IDX);
				p_area_info = target_get_chip_area(p_param, FUSE_IDX);
				p_area_info->size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_size"))
			{
				target_para_size_defined |= LOCK;
				target_assert_chip_area(p_param, LOCK_IDX);
				p_area_info = target_get_chip_area(p_param, LOCK_IDX);
				p_area_info->size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_size"))
			{
				target_para_size_defined |= BOOTLOADER;
				target_assert_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info = target_get_chip_area(p_param, BOOTLOADER_IDX);
				p_area_info->size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_size"))
			{
				target_para_size_defined |= RAM;
				target_assert_chip_area(p_param, RAM_IDX);
				p_area_info = target_get_chip_area(p_param, RAM_IDX);
				p_area_info->size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "uid_size"))
			{
				target_para_size_defined |= UNIQUEID;
				target_assert_chip_area(p_param, UNIQUEID_IDX);
				p_area_info = target_get_chip_area(p_param, UNIQUEID_IDX);
				p_area_info->size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_size"))
			{
				target_para_size_defined |= APPLICATION;
				target_assert_chip_area(p_param, APPLICATION_IDX);
				p_area_info = target_get_chip_area(p_param, APPLICATION_IDX);
				p_area_info->size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_size"))
			{
				target_para_size_defined |= EEPROM;
				target_assert_chip_area(p_param, EEPROM_IDX);
				p_area_info = target_get_chip_area(p_param, EEPROM_IDX);
				p_area_info->size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_size"))
			{
				target_para_size_defined |= OTPROM;
				target_assert_chip_area(p_param, OTPROM_IDX);
				p_area_info = target_get_chip_area(p_param, OTPROM_IDX);
				p_area_info->size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_size"))
			{
				target_para_size_defined |= USRSIG;
				target_assert_chip_area(p_param, USRSIG_IDX);
				p_area_info = target_get_chip_area(p_param, USRSIG_IDX);
				p_area_info->size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "uid"))
			{
				target_para_size_defined |= UNIQUEID;
				target_assert_chip_area(p_param, UNIQUEID_IDX);
				p_area_info = target_get_chip_area(p_param, UNIQUEID_IDX);
				if (target_parse_chip_area(p_area_info, paramNode))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse chip area");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse"))
			{
				target_para_size_defined |= FUSE;
				target_assert_chip_area(p_param, FUSE_IDX);
				p_area_info = target_get_chip_area(p_param, FUSE_IDX);
				if (target_parse_chip_area(p_area_info, paramNode))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse chip area");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock"))
			{
				target_para_size_defined |= LOCK;
				target_assert_chip_area(p_param, LOCK_IDX);
				p_area_info = target_get_chip_area(p_param, LOCK_IDX);
				if (target_parse_chip_area(p_area_info, paramNode))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse chip area");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "calibration"))
			{
				target_para_size_defined |= CALIBRATION;
				target_assert_chip_area(p_param, CALIBRATION_IDX);
				p_area_info = target_get_chip_area(p_param, CALIBRATION_IDX);
				if (target_parse_chip_area(p_area_info, paramNode))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse chip area");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig"))
			{
				target_para_size_defined |= USRSIG;
				target_assert_chip_area(p_param, USRSIG_IDX);
				p_area_info = target_get_chip_area(p_param, USRSIG_IDX);
				if (target_parse_chip_area(p_area_info, paramNode))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse chip area");
					err = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
			}
			else
			{
				char *str_tmp = (char *)paramNode->name;
				
				if ((strlen(str_tmp) >= 6)
					&& ('p' == str_tmp[0])
					&& ('a' == str_tmp[1])
					&& ('r' == str_tmp[2])
					&& ('a' == str_tmp[3])
					&& ('m' == str_tmp[4]))
				{
					// parameters
					j = strtoul(&str_tmp[5], NULL, 0);
					if (j >= dimof(p_param->param))
					{
						LOG_ERROR(ERRMSG_INVALID_TARGET, "param index");
						err = ERRCODE_FAILURE_OPERATION;
						goto free_and_exit;
					}
					p_param->param[j] = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				}
				else
				{
					// wrong parameter
					LOG_ERROR(ERRMSG_INVALID,
						(const char *)xmlNodeGetContent(paramNode),
						target->name);
					err = ERRCODE_INVALID;
					goto free_and_exit;
				}
			}
			
			paramNode = paramNode->next->next;
		}
		
		p_area_info = p_param->chip_areas;
		while (p_area_info != NULL)
		{
			if (!(target_para_size_defined & (1 << p_area_info->index)) &&
				!p_area_info->size)
			{
				p_area_info->size =
							p_area_info->page_size * p_area_info->page_num;
			}
			p_area_info = sllist_get_container(p_area_info->list.next,
												struct chip_area_info_t, list);
		}
		
		if (0 == i)
		{
			// first chip is used to setting every chip
			for (j = 1; j < s->num_of_chips; j++)
			{
				memcpy(&s->chips_param[j], &s->chips_param[0],
							sizeof(struct chip_param_t));
				s->chips_param[j].chip_name = NULL;
				s->chips_param[j].program_mode_str = NULL;
				s->chips_param[j].chip_areas =
					target_chip_area_dup(s->chips_param[0].chip_areas);
				
				p_area_info = s->chips_param[j].chip_areas;
				while (p_area_info != NULL)
				{
					p_area_info->mask = NULL;
					p_area_info->cli_format = NULL;
					p_area_info = sllist_get_container(p_area_info->list.next,
												struct chip_area_info_t, list);
				}
			}
		}
		
		curNode = curNode->next->next;
	}
	
free_and_exit:
	if (filename != NULL)
	{
		free(filename);
		filename = NULL;
	}
	if (doc != NULL)
	{
		xmlFreeDoc(doc);
		doc = NULL;
	}
	
	return err;
}

vsf_err_t target_generate_cfg_data(struct target_cfg_data_info_t *cfg_data_info,
									const char *filename)
{
	struct chip_area_info_t *area_info = NULL;
	struct chip_series_t target_chips = {NULL, 0, 0, NULL};
	FILE *cfgfile;
	bool little;
	uint8_t align;
	uint32_t offset, addrwidth;
	uint32_t info_size, data_size, all_size;
	uint32_t param_size, area_size, series_size, temp_len;
	uint32_t chip_area_pos, data_pos, temp_pos;
	uint32_t i, j, k;
	uint8_t *buff = NULL, *buff_ptr = NULL;
	uint8_t *chip_area_ptr = NULL;
	
	if ((NULL == cfg_data_info) || (NULL == filename) ||
		(cfg_data_info->align != 4) || (cfg_data_info->addr_width != 32))
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	offset = cfg_data_info->addr;
	addrwidth = cfg_data_info->addr_width / 8;
	align = cfg_data_info->align;
	little = cfg_data_info->little_endian;
	
	cfgfile = fopen(filename, "wb");
	if (NULL == cfgfile)
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "open", filename);
		return VSFERR_FAIL;
	}
	
	// struct chip_series_t
	series_size = 2 * addrwidth + 2 * 4;
	// struct chip_param_t
	param_size = 3 * addrwidth + 35 * 4;
	// struct chip_area_info_t
	area_size = 3 * addrwidth + 8 * 4;

	for (i = 0; targets_info[i].name != NULL; i++)
	{
		target_chips.series_name = NULL;
		target_chips.size = 0;
		target_chips.num_of_chips = 0;
		target_chips.chips_param = NULL;
		if (target_build_chip_series(
				(struct target_info_t *)&targets_info[i],
				targets_info[i].program_mode, &target_chips))
		{
			target_release_chip_series(&target_chips);
			continue;
		}
		
		#define DATASIZE(len)	(((len) + align - 1) / align) * align
		if (NULL == target_chips.series_name)
		{
			target_release_chip_series(&target_chips);
			continue;
		}
		
		info_size = series_size + target_chips.num_of_chips * param_size;
		chip_area_pos = info_size;
		temp_len = strlen(target_chips.series_name) + 1;
		data_size = DATASIZE(temp_len);
		for (j = 0; j < target_chips.num_of_chips; j++)
		{
			if (NULL == target_chips.chips_param[j].chip_name)
			{
				target_release_chip_series(&target_chips);
				continue;
			}
			temp_len = strlen(target_chips.chips_param[j].chip_name) + 1;
			data_size += DATASIZE(temp_len);
			if (target_chips.chips_param[j].program_mode_str != NULL)
			{
				temp_len = strlen(target_chips.chips_param[j].program_mode_str) + 1;
				data_size += DATASIZE(temp_len);
			}
		}
		for (j = 0; j < target_chips.num_of_chips; j++)
		{
			uint32_t default_value_addr;
			
			area_info = target_chips.chips_param[j].chip_areas;
			while (area_info != NULL)
			{
				default_value_addr = info_size + 5 * sizeof(uint32_t);
				info_size += area_size +
					(((default_value_addr + 7) & ~7) - default_value_addr);
				
				if (area_info->cli_format != NULL)
				{
					temp_len = 1 + strlen(area_info->cli_format);
					data_size += DATASIZE(temp_len);
				}
				if (area_info->mask != NULL)
				{
					temp_len = area_info->size;
					data_size += DATASIZE(temp_len);
				}
				
				area_info = sllist_get_container(area_info->list.next,
												struct chip_area_info_t, list);
			}
		}
		
		all_size = ((info_size + data_size) + 1023) & ~0x3FF;
		buff = (uint8_t *)malloc(all_size);
		if (NULL == buff)
		{
			fclose(cfgfile);
			cfgfile = NULL;
			target_release_chip_series(&target_chips);
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		memset(buff, 0, all_size);
		buff_ptr = buff;
		
		#define SET_U8(p, v)					\
			do {\
				*(uint8_t *)(p) = (uint8_t)(v);\
				(p) += 1;\
			} while (0)
		#define SET_U16(p, v)					\
			do {\
				if (little)\
				{\
					SET_LE_U16((p), (v));\
				}\
				else\
				{\
					SET_BE_U16((p), (v));\
				}\
				(p) += 2;\
			} while (0)
		#define SET_U32(p, v)					\
			do {\
				if (little)\
				{\
					SET_LE_U32((p), (v));\
				}\
				else\
				{\
					SET_BE_U32((p), (v));\
				}\
				(p) += 4;\
			} while (0)
		#define SET_U64(p, v)					\
			do {\
				if (little)\
				{\
					SET_LE_U64((p), (v));\
				}\
				else\
				{\
					SET_BE_U64((p), (v));\
				}\
				(p) += 8;\
			} while (0)
		#define SET_ABS_PTR(p, v)				\
			do {\
				switch (addrwidth)\
				{\
				case 1:\
					SET_U8((p), (v));\
					break;\
				case 2:\
					SET_U16((p), (v));\
					break;\
				case 4:\
					SET_U32((p), (v));\
					break;\
				case 8:\
					SET_U64((p), (v));\
					break;\
				}\
			} while (0)
		#define SET_PTR(p, v)					SET_ABS_PTR((p), offset + (v))
		#define SET_DATA_PTR(p, v, size)		\
			do {\
				SET_PTR((p), (v));\
				data_pos += size;\
			} while (0)
		
		data_pos = info_size;
		chip_area_ptr = &buff[chip_area_pos];
		//	struct chip_series_t
		//	{
		//		char *series_name;
		temp_len = DATASIZE(strlen(target_chips.series_name) + 1);
		SET_DATA_PTR(buff_ptr, data_pos, temp_len);
		//		uint32_t size;
		SET_U32(buff_ptr, all_size);
		//		uint32_t num_of_chips;
		SET_U32(buff_ptr, target_chips.num_of_chips);
		//		struct chip_param_t *chips_param;
		//	}
		SET_PTR(buff_ptr, series_size);
		
		for (j = 0; j < target_chips.num_of_chips; j++)
		{
			//	struct chip_param_t
			//	{
			//		char *chip_name;
			temp_len = DATASIZE(strlen(target_chips.chips_param[j].chip_name) + 1);
			SET_DATA_PTR(buff_ptr, data_pos, temp_len);
			//		uint32_t chip_id;
			SET_U32(buff_ptr, target_chips.chips_param[j].chip_id);
			//		char *program_mode_str;
			if (target_chips.chips_param[j].program_mode_str != NULL)
			{
				temp_len = DATASIZE(strlen(target_chips.chips_param[j].program_mode_str) + 1);
				SET_DATA_PTR(buff_ptr, data_pos, temp_len);
			}
			else
			{
				SET_ABS_PTR(buff_ptr, 0);
			}
			//		uint32_t program_mode;
			SET_U32(buff_ptr, target_chips.chips_param[j].program_mode);
			//		uint32_t chip_erase;
			SET_U32(buff_ptr, target_chips.chips_param[j].chip_erase);
			//		uint32_t param[32];
			for (k = 0; k < dimof(target_chips.chips_param[j].param); k++)
			{
				SET_U32(buff_ptr, target_chips.chips_param[j].param[k]);
			}
			//		struct chip_area_info_t *chip_areas;
			SET_PTR(buff_ptr, chip_area_ptr - buff);
			
			area_info = target_chips.chips_param[j].chip_areas;
			while (area_info != NULL)
			{
				//	struct chip_area_info_t
				//	{
				//		uint32_t index;
				SET_U32(chip_area_ptr, area_info->index);
				//		uint32_t seg;
				SET_U32(chip_area_ptr, area_info->seg);
				//		uint32_t addr;
				SET_U32(chip_area_ptr, area_info->addr);
				//		uint32_t page_size;
				SET_U32(chip_area_ptr, area_info->page_size);
				//		uint32_t page_num;
				SET_U32(chip_area_ptr, area_info->page_num);
				//		uint64_t default_value;
				//		should be 64-bit aligned
				temp_pos = chip_area_ptr - buff;
				temp_pos = ((temp_pos + 7) & ~7) - temp_pos;
				chip_area_ptr += temp_pos;
				SET_U64(chip_area_ptr, area_info->default_value);
				//		uint32_t size;
				SET_U32(chip_area_ptr, area_info->size);
				//		uint8_t *mask;
				if (area_info->mask != NULL)
				{
					temp_len = DATASIZE(area_info->size);
					SET_DATA_PTR(chip_area_ptr, data_pos, temp_len);
				}
				else
				{
					SET_ABS_PTR(chip_area_ptr, 0);
				}
				//		char *cli_format;
				if (area_info->cli_format != NULL)
				{
					temp_len = DATASIZE(strlen(area_info->cli_format) + 1);
					SET_DATA_PTR(chip_area_ptr, data_pos, temp_len);
				}
				else
				{
					SET_ABS_PTR(chip_area_ptr, 0);
				}
				
				area_info = sllist_get_container(area_info->list.next,
												struct chip_area_info_t, list);
				if (area_info != NULL)
				{
					//		struct sllist list;
					temp_pos = chip_area_ptr - buff;
					temp_pos += addrwidth + 5 * sizeof(uint32_t);
					temp_pos = (temp_pos + 7) & ~7;
					temp_pos += 2 * addrwidth + sizeof(uint32_t) +
								sizeof(uint64_t);
					SET_PTR(chip_area_ptr, temp_pos);
				}
				else
				{
					//		struct sllist list;
					SET_ABS_PTR(chip_area_ptr, 0);
				}
			}
		}
		
		#define SET_DATA(p, src, copy_len, len)	\
			do {\
				memcpy((p), (src), (copy_len));\
				(p) += (len);\
			} while (0)
		
		buff_ptr = &buff[info_size];
		//		chip_series_t.series_name
		temp_len = strlen(target_chips.series_name) + 1;
		SET_DATA(buff_ptr, target_chips.series_name, temp_len, DATASIZE(temp_len));
		for (j = 0; j < target_chips.num_of_chips; j++)
		{
			//		chip_param_t.chip_name
			temp_len = strlen(target_chips.chips_param[j].chip_name) + 1;
			SET_DATA(buff_ptr, target_chips.chips_param[j].chip_name, temp_len, DATASIZE(temp_len));
			//		char *program_mode_str;
			if (target_chips.chips_param[j].program_mode_str != NULL)
			{
				temp_len = strlen(target_chips.chips_param[j].program_mode_str) + 1;
				SET_DATA(buff_ptr, target_chips.chips_param[j].program_mode_str, temp_len, DATASIZE(temp_len));
			}
			
			//		struct chip_area_info_t *chip_areas
			area_info = target_chips.chips_param[j].chip_areas;
			while (area_info != NULL)
			{
				//		uint8_t *mask;
				if (area_info->mask != NULL)
				{
					temp_len = area_info->size;
					SET_DATA(buff_ptr, area_info->mask, temp_len, DATASIZE(temp_len));
				}
				//		char *cli_format;
				if (area_info->cli_format != NULL)
				{
					temp_len = strlen(area_info->cli_format) + 1;
					SET_DATA(buff_ptr, area_info->cli_format, temp_len, DATASIZE(temp_len));
				}
				
				area_info = sllist_get_container(area_info->list.next,
												struct chip_area_info_t, list);
			}
		}
		
		if (fwrite(buff, 1, all_size, cfgfile) != all_size)
		{
			free(buff);
			buff = NULL;
			fclose(cfgfile);
			cfgfile = NULL;
			target_release_chip_series(&target_chips);
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write data to file");
			return VSFERR_FAIL;
		}
		
		free(buff);
		buff = NULL;
		target_release_chip_series(&target_chips);
		offset += all_size;
	}
	
	// write dummy series as end
	series_size = (series_size + 1023) & ~0x3FF;
	buff = (uint8_t *)malloc(series_size);
	if (NULL == buff)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	memset(buff, 0, series_size);
	if (fwrite(buff, 1, series_size, cfgfile) != series_size)
	{
		free(buff);
		buff = NULL;
		fclose(cfgfile);
		cfgfile = NULL;
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write data to file");
		return VSFERR_FAIL;
	}
	free(buff);
	buff = NULL;
	
	fclose(cfgfile);
	cfgfile = NULL;
	return VSFERR_NONE;
}

