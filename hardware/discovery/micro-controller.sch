EESchema Schematic File Version 2
LIBS:poss_discovery-rescue
LIBS:power
LIBS:device
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:stm32f4_disco_header
LIBS:74xgxx
LIBS:ac-dc
LIBS:brooktre
LIBS:cmos_ieee
LIBS:dc-dc
LIBS:elec-unifil
LIBS:ftdi
LIBS:gennum
LIBS:graphic
LIBS:logo
LIBS:microchip_pic10mcu
LIBS:microchip_pic12mcu
LIBS:microchip_pic16mcu
LIBS:microchip1
LIBS:msp430
LIBS:nxp_armmcu
LIBS:powerint
LIBS:pspice
LIBS:references
LIBS:relays
LIBS:sensors
LIBS:special
LIBS:stm8
LIBS:stm32
LIBS:supertex
LIBS:transf
LIBS:ttl_ieee
LIBS:video
LIBS:poss_discovery-cache
EELAYER 25 0
EELAYER END
$Descr A3 16535 11693
encoding utf-8
Sheet 3 5
Title "Pocket Open Source Syntesizer - Discovery"
Date ""
Rev "B"
Comp "Fabien Chouteau"
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
Text GLabel 2850 5000 0    60   Input ~ 0
VBUS
Text GLabel 2850 5400 0    60   Input ~ 0
SWDIO
Text GLabel 2850 5500 0    60   Input ~ 0
SWCLK
Text GLabel 2850 6500 0    60   Input ~ 0
Audio_SCL
Text GLabel 2850 6600 0    60   Input ~ 0
Audio_SDA
Text GLabel 2850 6200 0    60   Input ~ 0
I2S_SCLK
Text GLabel 2850 6300 0    60   Input ~ 0
I2S_DOUT
Text GLabel 2850 6400 0    60   Input ~ 0
I2S_DIN
Text GLabel 2850 5200 0    60   Input ~ 0
FS_DM
Text GLabel 2850 5300 0    60   Input ~ 0
FS_DP
$Comp
L GND #PWR15
U 1 1 585E3358
P 7650 7850
F 0 "#PWR15" H 7650 7850 30  0001 C CNN
F 1 "GND" H 7650 7780 30  0001 C CNN
F 2 "" H 7650 7850 60  0001 C CNN
F 3 "" H 7650 7850 60  0001 C CNN
	1    7650 7850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR17
U 1 1 585E336D
P 7850 7850
F 0 "#PWR17" H 7850 7850 30  0001 C CNN
F 1 "GND" H 7850 7780 30  0001 C CNN
F 2 "" H 7850 7850 60  0001 C CNN
F 3 "" H 7850 7850 60  0001 C CNN
	1    7850 7850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR18
U 1 1 585E337B
P 8050 7850
F 0 "#PWR18" H 8050 7850 30  0001 C CNN
F 1 "GND" H 8050 7780 30  0001 C CNN
F 2 "" H 8050 7850 60  0001 C CNN
F 3 "" H 8050 7850 60  0001 C CNN
	1    8050 7850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR19
U 1 1 585E3389
P 8250 7850
F 0 "#PWR19" H 8250 7850 30  0001 C CNN
F 1 "GND" H 8250 7780 30  0001 C CNN
F 2 "" H 8250 7850 60  0001 C CNN
F 3 "" H 8250 7850 60  0001 C CNN
	1    8250 7850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR14
U 1 1 585E3397
P 7450 7850
F 0 "#PWR14" H 7450 7850 30  0001 C CNN
F 1 "GND" H 7450 7780 30  0001 C CNN
F 2 "" H 7450 7850 60  0001 C CNN
F 3 "" H 7450 7850 60  0001 C CNN
	1    7450 7850
	1    0    0    -1  
$EndComp
$Comp
L C C4
U 1 1 585E3587
P 8750 8050
F 0 "C4" H 8750 8150 40  0000 L CNN
F 1 "2.2uF" H 8756 7965 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 8788 7900 30  0001 C CNN
F 3 "" H 8750 8050 60  0000 C CNN
	1    8750 8050
	1    0    0    -1  
$EndComp
$Comp
L C C5
U 1 1 585E35B8
P 9250 8050
F 0 "C5" H 9250 8150 40  0000 L CNN
F 1 "2.2uF" H 9256 7965 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 9288 7900 30  0001 C CNN
F 3 "" H 9250 8050 60  0000 C CNN
	1    9250 8050
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR20
U 1 1 585E35DF
P 8750 8250
F 0 "#PWR20" H 8750 8250 30  0001 C CNN
F 1 "GND" H 8750 8180 30  0001 C CNN
F 2 "" H 8750 8250 60  0001 C CNN
F 3 "" H 8750 8250 60  0001 C CNN
	1    8750 8250
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR21
U 1 1 585E35F3
P 9250 8250
F 0 "#PWR21" H 9250 8250 30  0001 C CNN
F 1 "GND" H 9250 8180 30  0001 C CNN
F 2 "" H 9250 8250 60  0001 C CNN
F 3 "" H 9250 8250 60  0001 C CNN
	1    9250 8250
	1    0    0    -1  
$EndComp
Text GLabel 2850 4100 0    60   Input ~ 0
WAKEUP
$Comp
L CRYSTAL X1
U 1 1 585F8666
P 2300 3000
F 0 "X1" H 2300 3150 60  0000 C CNN
F 1 "CRYSTAL" H 2300 2850 60  0000 C CNN
F 2 "Crystals:Crystal_HC49-U_Vertical" H 2300 3000 60  0001 C CNN
F 3 "" H 2300 3000 60  0000 C CNN
	1    2300 3000
	0    1    1    0   
$EndComp
$Comp
L C C12
U 1 1 585F8833
P 1750 2700
F 0 "C12" H 1750 2800 40  0000 L CNN
F 1 "C" H 1756 2615 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 1788 2550 30  0001 C CNN
F 3 "" H 1750 2700 60  0000 C CNN
	1    1750 2700
	0    -1   -1   0   
$EndComp
$Comp
L C C13
U 1 1 585F888D
P 1750 3300
F 0 "C13" H 1750 3400 40  0000 L CNN
F 1 "C" H 1756 3215 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 1788 3150 30  0001 C CNN
F 3 "" H 1750 3300 60  0000 C CNN
	1    1750 3300
	0    -1   -1   0   
$EndComp
$Comp
L GND #PWR8
U 1 1 585F8908
P 1300 3050
F 0 "#PWR8" H 1300 3050 30  0001 C CNN
F 1 "GND" H 1300 2980 30  0001 C CNN
F 2 "" H 1300 3050 60  0001 C CNN
F 3 "" H 1300 3050 60  0001 C CNN
	1    1300 3050
	1    0    0    -1  
$EndComp
$Comp
L C C22
U 1 1 5860FE12
P 8600 1500
F 0 "C22" H 8600 1600 40  0000 L CNN
F 1 "0.1uF" H 8606 1415 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 8638 1350 30  0001 C CNN
F 3 "" H 8600 1500 60  0000 C CNN
	1    8600 1500
	1    0    0    -1  
$EndComp
$Comp
L C C21
U 1 1 5860FECB
P 8300 1500
F 0 "C21" H 8300 1600 40  0000 L CNN
F 1 "0.1uF" H 8306 1415 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 8338 1350 30  0001 C CNN
F 3 "" H 8300 1500 60  0000 C CNN
	1    8300 1500
	1    0    0    -1  
$EndComp
$Comp
L C C20
U 1 1 5860FFD1
P 8000 1500
F 0 "C20" H 8000 1600 40  0000 L CNN
F 1 "0.1uF" H 8006 1415 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 8038 1350 30  0001 C CNN
F 3 "" H 8000 1500 60  0000 C CNN
	1    8000 1500
	1    0    0    -1  
$EndComp
$Comp
L C C19
U 1 1 5860FFD7
P 7700 1500
F 0 "C19" H 7700 1600 40  0000 L CNN
F 1 "0.1uF" H 7706 1415 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 7738 1350 30  0001 C CNN
F 3 "" H 7700 1500 60  0000 C CNN
	1    7700 1500
	1    0    0    -1  
$EndComp
$Comp
L C C18
U 1 1 58610033
P 7400 1500
F 0 "C18" H 7400 1600 40  0000 L CNN
F 1 "0.1uF" H 7406 1415 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 7438 1350 30  0001 C CNN
F 3 "" H 7400 1500 60  0000 C CNN
	1    7400 1500
	1    0    0    -1  
$EndComp
$Comp
L C C17
U 1 1 58610039
P 7100 1500
F 0 "C17" H 7100 1600 40  0000 L CNN
F 1 "0.1uF" H 7106 1415 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 7138 1350 30  0001 C CNN
F 3 "" H 7100 1500 60  0000 C CNN
	1    7100 1500
	1    0    0    -1  
$EndComp
$Comp
L C C16
U 1 1 586106F3
P 6800 1500
F 0 "C16" H 6800 1600 40  0000 L CNN
F 1 "22uF" H 6806 1415 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 6838 1350 30  0001 C CNN
F 3 "" H 6800 1500 60  0000 C CNN
	1    6800 1500
	1    0    0    -1  
$EndComp
$Comp
L C C15
U 1 1 586109F3
P 6100 1500
F 0 "C15" H 6100 1600 40  0000 L CNN
F 1 "0.1uF" H 6106 1415 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 6138 1350 30  0001 C CNN
F 3 "" H 6100 1500 60  0000 C CNN
	1    6100 1500
	1    0    0    -1  
$EndComp
Text GLabel 9100 1750 2    60   Input ~ 0
3V3
$Comp
L C C14
U 1 1 586115D4
P 5600 1500
F 0 "C14" H 5600 1600 40  0000 L CNN
F 1 "1uF" H 5606 1415 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 5638 1350 30  0001 C CNN
F 3 "" H 5600 1500 60  0000 C CNN
	1    5600 1500
	1    0    0    -1  
$EndComp
$Comp
L INDUCTOR_SMALL L1
U 1 1 5861179B
P 5050 1750
F 0 "L1" H 5050 1850 50  0000 C CNN
F 1 "15Ohm" H 5050 1700 50  0000 C CNN
F 2 "Capacitors_SMD:C_0805" H 5050 1750 60  0001 C CNN
F 3 "" H 5050 1750 60  0000 C CNN
	1    5050 1750
	1    0    0    -1  
$EndComp
Text GLabel 4550 1750 0    60   Input ~ 0
3V3
$Comp
L R R5
U 1 1 58612338
P 2600 2300
F 0 "R5" V 2680 2300 40  0000 C CNN
F 1 "10K" V 2607 2301 40  0000 C CNN
F 2 "Resistors_SMD:R_0805" V 2530 2300 30  0001 C CNN
F 3 "" H 2600 2300 30  0000 C CNN
	1    2600 2300
	0    1    1    0   
$EndComp
$Comp
L R R6
U 1 1 586123C6
P 2600 2500
F 0 "R6" V 2680 2500 40  0000 C CNN
F 1 "10K" V 2607 2501 40  0000 C CNN
F 2 "Resistors_SMD:R_0805" V 2530 2500 30  0001 C CNN
F 3 "" H 2600 2500 30  0000 C CNN
	1    2600 2500
	0    1    1    0   
$EndComp
$Comp
L GND #PWR10
U 1 1 58612418
P 2350 2300
F 0 "#PWR10" H 2350 2300 30  0001 C CNN
F 1 "GND" H 2350 2230 30  0001 C CNN
F 2 "" H 2350 2300 60  0001 C CNN
F 3 "" H 2350 2300 60  0001 C CNN
	1    2350 2300
	0    1    1    0   
$EndComp
Text GLabel 2350 2500 0    60   Input ~ 0
3V3
$Comp
L GND #PWR11
U 1 1 58612E50
P 5450 1150
F 0 "#PWR11" H 5450 1150 30  0001 C CNN
F 1 "GND" H 5450 1080 30  0001 C CNN
F 2 "" H 5450 1150 60  0001 C CNN
F 3 "" H 5450 1150 60  0001 C CNN
	1    5450 1150
	0    1    1    0   
$EndComp
$Comp
L GND #PWR9
U 1 1 5861353A
P 1900 6300
F 0 "#PWR9" H 1900 6300 30  0001 C CNN
F 1 "GND" H 1900 6230 30  0001 C CNN
F 2 "" H 1900 6300 60  0001 C CNN
F 3 "" H 1900 6300 60  0001 C CNN
	1    1900 6300
	1    0    0    -1  
$EndComp
$Comp
L STM32F407VG U4
U 1 1 585DBF7E
P 7850 4850
F 0 "U4" H 7850 4850 60  0000 C CNN
F 1 "STM32F407VG" H 7850 4950 60  0000 C CNN
F 2 "Housings_QFP:LQFP-100_14x14mm_Pitch0.5mm" H 7850 4750 60  0000 C CNN
F 3 "" H 7850 4850 60  0001 C CNN
	1    7850 4850
	1    0    0    -1  
$EndComp
Text GLabel 1850 8050 1    60   Input ~ 0
3V3
$Comp
L R R3
U 1 1 586150DA
P 1650 8650
F 0 "R3" V 1730 8650 40  0000 C CNN
F 1 "10K" V 1657 8651 40  0000 C CNN
F 2 "Resistors_SMD:R_0805" V 1580 8650 30  0001 C CNN
F 3 "" H 1650 8650 30  0000 C CNN
	1    1650 8650
	1    0    0    -1  
$EndComp
$Comp
L R R4
U 1 1 58615169
P 2050 8650
F 0 "R4" V 2130 8650 40  0000 C CNN
F 1 "10K" V 2057 8651 40  0000 C CNN
F 2 "Resistors_SMD:R_0805" V 1980 8650 30  0001 C CNN
F 3 "" H 2050 8650 30  0000 C CNN
	1    2050 8650
	1    0    0    -1  
$EndComp
Text GLabel 1650 8900 3    60   Input ~ 0
Audio_SDA
Text GLabel 2050 8900 3    60   Input ~ 0
Audio_SCL
Text GLabel 2850 5600 0    60   Input ~ 0
I2S_LRCLK
Text GLabel 12850 6600 2    60   Input ~ 0
I2S_MCLK
Text GLabel 12850 6700 2    60   Input ~ 0
SD_DAT0
Text GLabel 12850 6500 2    60   Input ~ 0
Play
Text GLabel 12850 5600 2    60   Input ~ 0
FX
Text GLabel 12850 5500 2    60   Input ~ 0
COL9
Text GLabel 12850 5400 2    60   Input ~ 0
ROW1
Text GLabel 12850 5300 2    60   Input ~ 0
ROW2
Text GLabel 12850 5200 2    60   Input ~ 0
ROW3
Text GLabel 12850 5100 2    60   Input ~ 0
B16
Text GLabel 12850 5000 2    60   Input ~ 0
B8
Text GLabel 12850 4900 2    60   Input ~ 0
BPM_VOL
Text GLabel 2850 7400 0    60   Input ~ 0
COL8
Text GLabel 2850 7100 0    60   Input ~ 0
B15
Text GLabel 2850 7200 0    60   Input ~ 0
B7
Text GLabel 2850 7300 0    60   Input ~ 0
BE
Text GLabel 2850 7000 0    60   Input ~ 0
COL7
Text GLabel 2850 6900 0    60   Input ~ 0
B14
Text GLabel 12850 3800 2    60   Input ~ 0
B6
Text GLabel 12850 3700 2    60   Input ~ 0
BD
Text GLabel 12850 3600 2    60   Input ~ 0
COL6
Text GLabel 12850 3500 2    60   Input ~ 0
B13
Text GLabel 12850 3400 2    60   Input ~ 0
B5
Text GLabel 12850 3300 2    60   Input ~ 0
BC
Text GLabel 12850 3200 2    60   Input ~ 0
COL5
$Comp
L MOLEX_microSD_47219-2001 U8
U 1 1 58639490
P 7350 8750
F 0 "U8" H 7350 8750 60  0000 C CNN
F 1 "MOLEX_microSD_47219-2001" V 8350 7850 60  0000 C CNN
F 2 "stm32f4-disco:MOLEX_microSD_47219-2001" H 7350 8750 60  0001 C CNN
F 3 "" H 7350 8750 60  0001 C CNN
	1    7350 8750
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR16
U 1 1 586394FE
P 7800 10350
F 0 "#PWR16" H 7800 10350 30  0001 C CNN
F 1 "GND" H 7800 10280 30  0001 C CNN
F 2 "" H 7800 10350 60  0001 C CNN
F 3 "" H 7800 10350 60  0001 C CNN
	1    7800 10350
	1    0    0    -1  
$EndComp
Text GLabel 4950 9850 0    60   Input ~ 0
SD_DAT2
Text GLabel 4950 9700 0    60   Input ~ 0
SD_DAT3
Text GLabel 4950 9400 0    60   Input ~ 0
SD_CMD
Text GLabel 4950 10150 0    60   Input ~ 0
SD_DAT0
Text GLabel 4950 10000 0    60   Input ~ 0
SD_DAT1
Text GLabel 7350 8550 1    60   Input ~ 0
3V3
Text GLabel 12850 7000 2    60   Input ~ 0
SD_DAT3
Text GLabel 12850 6800 2    60   Input ~ 0
SD_DAT1
Text GLabel 12850 6900 2    60   Input ~ 0
SD_DAT2
Text GLabel 2850 5100 0    60   Input ~ 0
Write
Text GLabel 12850 7100 2    60   Input ~ 0
SD_CLK
Text GLabel 12850 4300 2    60   Input ~ 0
SD_CMD
$Comp
L GND #PWR13
U 1 1 58631320
P 6300 9500
F 0 "#PWR13" H 6300 9500 30  0001 C CNN
F 1 "GND" H 6300 9430 30  0001 C CNN
F 2 "" H 6300 9500 60  0001 C CNN
F 3 "" H 6300 9500 60  0001 C CNN
	1    6300 9500
	1    0    0    -1  
$EndComp
Text GLabel 12850 3100 2    60   Input ~ 0
B12
Text GLabel 12850 3000 2    60   Input ~ 0
B4
Text GLabel 2850 6000 0    60   Input ~ 0
BB
Text GLabel 2850 5900 0    60   Input ~ 0
COL4
Text GLabel 12850 6400 2    60   Input ~ 0
B11
Text GLabel 12850 6300 2    60   Input ~ 0
B3
Text GLabel 2850 4800 0    60   Input ~ 0
BA
Text GLabel 2850 4700 0    60   Input ~ 0
COL3
Text GLabel 2850 4600 0    60   Input ~ 0
B10
Text GLabel 2850 4500 0    60   Input ~ 0
B9
Text GLabel 2850 4400 0    60   Input ~ 0
B2
Text GLabel 2850 4300 0    60   Input ~ 0
COL2
Text GLabel 2850 4200 0    60   Input ~ 0
B1
Text GLabel 12850 6100 2    60   Input ~ 0
COL1
$Comp
L R R10
U 1 1 5863DABF
P 5200 9400
F 0 "R10" V 5280 9400 40  0000 C CNN
F 1 "10K" V 5207 9401 40  0000 C CNN
F 2 "Resistors_SMD:R_0805" V 5130 9400 30  0001 C CNN
F 3 "" H 5200 9400 30  0000 C CNN
	1    5200 9400
	0    1    1    0   
$EndComp
$Comp
L R R11
U 1 1 5863DC3C
P 5200 9700
F 0 "R11" V 5280 9700 40  0000 C CNN
F 1 "10K" V 5207 9701 40  0000 C CNN
F 2 "Resistors_SMD:R_0805" V 5130 9700 30  0001 C CNN
F 3 "" H 5200 9700 30  0000 C CNN
	1    5200 9700
	0    1    1    0   
$EndComp
$Comp
L R R12
U 1 1 5863DC9E
P 5200 9850
F 0 "R12" V 5280 9850 40  0000 C CNN
F 1 "10K" V 5207 9851 40  0000 C CNN
F 2 "Resistors_SMD:R_0805" V 5130 9850 30  0001 C CNN
F 3 "" H 5200 9850 30  0000 C CNN
	1    5200 9850
	0    1    1    0   
$EndComp
$Comp
L R R13
U 1 1 5863DE93
P 5200 10000
F 0 "R13" V 5280 10000 40  0000 C CNN
F 1 "10K" V 5207 10001 40  0000 C CNN
F 2 "Resistors_SMD:R_0805" V 5130 10000 30  0001 C CNN
F 3 "" H 5200 10000 30  0000 C CNN
	1    5200 10000
	0    1    1    0   
$EndComp
$Comp
L R R14
U 1 1 5863DEFD
P 5200 10150
F 0 "R14" V 5280 10150 40  0000 C CNN
F 1 "10K" V 5207 10151 40  0000 C CNN
F 2 "Resistors_SMD:R_0805" V 5130 10150 30  0001 C CNN
F 3 "" H 5200 10150 30  0000 C CNN
	1    5200 10150
	0    1    1    0   
$EndComp
Text GLabel 5450 9100 1    60   Input ~ 0
3V3
Text GLabel 7250 9850 0    60   Input ~ 0
SD_DAT2
Text GLabel 7250 9700 0    60   Input ~ 0
SD_DAT3
Text GLabel 7250 9400 0    60   Input ~ 0
SD_CMD
Text GLabel 7250 10150 0    60   Input ~ 0
SD_DAT0
Text GLabel 7250 10000 0    60   Input ~ 0
SD_DAT1
Text GLabel 7250 9550 0    60   Input ~ 0
SD_CLK
$Comp
L C C23
U 1 1 5863FDE0
P 5450 10425
F 0 "C23" H 5450 10525 40  0000 L CNN
F 1 "0.1uF" H 5456 10340 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 5488 10275 30  0001 C CNN
F 3 "" H 5450 10425 60  0000 C CNN
	1    5450 10425
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR12
U 1 1 58640311
P 5450 10625
F 0 "#PWR12" H 5450 10625 30  0001 C CNN
F 1 "GND" H 5450 10555 30  0001 C CNN
F 2 "" H 5450 10625 60  0001 C CNN
F 3 "" H 5450 10625 60  0001 C CNN
	1    5450 10625
	1    0    0    -1  
$EndComp
Wire Wire Line
	1950 2700 2850 2700
Wire Wire Line
	1950 3300 2850 3300
Connection ~ 2300 2700
Connection ~ 2300 3300
Wire Wire Line
	1550 3300 1550 2700
Wire Wire Line
	1550 2950 1300 2950
Wire Wire Line
	1300 2950 1300 3050
Connection ~ 1550 2950
Wire Wire Line
	8600 1700 8600 1850
Wire Wire Line
	8300 1700 8300 1850
Wire Wire Line
	8000 1700 8000 1850
Wire Wire Line
	7700 1700 7700 1850
Wire Wire Line
	7400 1700 7400 1850
Wire Wire Line
	7100 1700 7100 1850
Wire Wire Line
	9100 1750 9100 1850
Connection ~ 8600 1750
Connection ~ 8300 1750
Connection ~ 8000 1750
Connection ~ 7700 1750
Connection ~ 7400 1750
Connection ~ 7100 1750
Wire Wire Line
	6800 1750 6800 1700
Wire Wire Line
	8600 1150 8600 1300
Wire Wire Line
	6800 1150 6800 1300
Connection ~ 6800 1750
Wire Wire Line
	6100 1700 6100 1850
Wire Wire Line
	6100 1150 6100 1300
Connection ~ 6800 1150
Wire Wire Line
	7100 1300 7100 1150
Connection ~ 7100 1150
Wire Wire Line
	7400 1300 7400 1150
Connection ~ 7400 1150
Wire Wire Line
	7700 1300 7700 1150
Connection ~ 7700 1150
Wire Wire Line
	8000 1300 8000 1150
Connection ~ 8000 1150
Wire Wire Line
	8300 1300 8300 1150
Connection ~ 8300 1150
Wire Wire Line
	9100 1750 6800 1750
Wire Wire Line
	5450 1150 8600 1150
Wire Wire Line
	5600 1150 5600 1300
Connection ~ 6100 1150
Wire Wire Line
	5600 1700 5600 1850
Wire Wire Line
	5300 1750 6100 1750
Connection ~ 5600 1750
Connection ~ 6100 1750
Wire Wire Line
	4800 1750 4550 1750
Connection ~ 5600 1150
Wire Wire Line
	2850 6100 1900 6100
Wire Wire Line
	1900 6100 1900 6300
Wire Wire Line
	1850 8050 1850 8250
Connection ~ 1850 8250
Wire Wire Line
	2050 8250 2050 8400
Wire Wire Line
	2050 8250 1650 8250
Wire Wire Line
	1650 8250 1650 8400
Wire Wire Line
	7250 9250 6300 9250
Wire Wire Line
	6300 9250 6300 9500
Wire Wire Line
	5450 9100 5450 10225
Connection ~ 5450 9850
Connection ~ 5450 10000
Connection ~ 5450 9700
Connection ~ 5450 9400
Connection ~ 5450 10150
$Comp
L C C24
U 1 1 58640860
P 6900 8875
F 0 "C24" H 6900 8975 40  0000 L CNN
F 1 "22uF" H 6906 8790 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 6938 8725 30  0001 C CNN
F 3 "" H 6900 8875 60  0000 C CNN
	1    6900 8875
	1    0    0    -1  
$EndComp
Wire Wire Line
	6900 9075 6900 9250
Connection ~ 6900 9250
Wire Wire Line
	7800 9000 7800 8675
Wire Wire Line
	7800 8675 6900 8675
Wire Wire Line
	7350 8550 7350 8675
Connection ~ 7350 8675
$EndSCHEMATC
