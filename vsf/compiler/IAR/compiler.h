#ifndef __COMPILER_H_INCLUDED__
#define __COMPILER_H_INCLUDED__

#include <string.h>
#include <intrinsics.h>

#define __VSF_FUNCNAME__				"cur_function"

#define ROOTFUNC	__root
#define PACKED_HEAD	__packed
#define PACKED_MID	
#define PACKED_TAIL	

#define vsf_enter_critical()			__disable_interrupt()
#define vsf_leave_critical()			__enable_interrupt()

#endif	// __COMPILER_H_INCLUDED__
