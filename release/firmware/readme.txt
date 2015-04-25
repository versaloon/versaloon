/release/firmware/MiniRelease1/STM32USBBoot.hex is the Bootloader emulating a COM port and implement the ISP protocol of STM32.
You can use any STM32 ISP downloader to update the firmware.
/release/firmware/MiniRelease1/Versaloon.hex is the latest firmware in hex format.

\release\firmware\MiniRelease1\NFW\MSCBoot.bin is the bootloader emulating a MSC device.
You can implement the firmware simply by copy a flash.bin to the emulated disc.
\release\firmware\MiniRelease1\NFW\flash.bin the latest firmware in binary format.

Note:
1. If you want to use the latest OpenOCD, please update the firmware to version later than:
for STM32USBBoot, SVN 1566
for MSCBoot, SVN 1562
