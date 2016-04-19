#define IS_WIN32			1
#define IS_MSVC				1

#define VERSION				"1.0"
#define PKGBLDDATE			__DATE__
#define RELSTR				"MSVC:"
#define PKGBLDREV			"0000"

#define access(a,b)			((GetFileAttributes(a) == 0xFFFFFFFF) ? 1 : 0)
//#define strtoull(s, p, r)	_strtoui64((s), (p), (r))
//#define snprintf			_snprintf

#pragma warning(disable:4127)
