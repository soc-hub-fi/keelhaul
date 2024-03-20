/*
 * Author: Henri Lunnikivi
 */
MEMORY
{
  /* This is actually two 0x8000 private L2 banks */
  L2 : ORIGIN = 0x1c000000, LENGTH = 0x10000
  BOOTRAM : ORIGIN = 0x10000, LENGTH = 0x8000
}

/* rodata seems to be the big section with the register addresses, let's put it in the largest contiguous block */
REGION_ALIAS("REGION_RODATA", L2);

REGION_ALIAS("REGION_DATA", BOOTRAM);
REGION_ALIAS("REGION_BSS", BOOTRAM);
REGION_ALIAS("REGION_HEAP", BOOTRAM);
REGION_ALIAS("REGION_STACK", BOOTRAM);
REGION_ALIAS("REGION_TEXT", BOOTRAM);
