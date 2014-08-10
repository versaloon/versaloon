// STLINK Commands
#define STLINK_CMD_START				0xF1
#define STLINK_CMD_END					0XF7

uint16_t stlink_getpkgsize(uint8_t *cmd, uint16_t len);
uint16_t stlink_process(uint8_t *cmd, uint16_t len);
