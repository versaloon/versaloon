#ifndef __TARGET_DATA_H_INCLUDED__
#define __TARGET_DATA_H_INCLUDED__

struct target_slot_t
{
	uint32_t data_base;
	uint32_t data_size;
	uint32_t script_base;
	uint32_t script_size;
};
extern uint8_t target_slotnum;
extern struct target_slot_t target_slot[8];
vsf_err_t target_init_slots(void);

#endif	// __TARGET_DATA_H_INCLUDED__