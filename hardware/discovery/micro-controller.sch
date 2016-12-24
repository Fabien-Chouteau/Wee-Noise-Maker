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
Text GLabel 2850 4200 0    60   Input ~ 0
POT1
Text GLabel 2850 4300 0    60   Input ~ 0
COL3
Text GLabel 2850 4400 0    60   Input ~ 0
POT2
Text GLabel 2850 4500 0    60   Input ~ 0
I2S_LRCLK
Text GLabel 2850 4600 0    60   Input ~ 0
SPI1_SCK
Text GLabel 2850 4700 0    60   Input ~ 0
SPI1_MISO
Text GLabel 2850 4800 0    60   Input ~ 0
SPI1_MOSI
Text GLabel 2850 4900 0    60   Input ~ 0
ROW5
Text GLabel 2850 5000 0    60   Input ~ 0
VBUS
Text GLabel 2850 5100 0    60   Input ~ 0
COL2
Text GLabel 2850 5400 0    60   Input ~ 0
SWDIO
Text GLabel 2850 5500 0    60   Input ~ 0
SWCLK
Text GLabel 2850 5600 0    60   Input ~ 0
ROW3
Text GLabel 2850 6200 0    60   Input ~ 0
B15
Text GLabel 2850 6300 0    60   Input ~ 0
B14
Text GLabel 2850 6400 0    60   Input ~ 0
B5
Text GLabel 2850 6500 0    60   Input ~ 0
Audio_SCL
Text GLabel 2850 6600 0    60   Input ~ 0
B9
Text GLabel 2850 6700 0    60   Input ~ 0
B4
Text GLabel 2850 6800 0    60   Input ~ 0
Audio_SDA
Text GLabel 2850 7400 0    60   Input ~ 0
BD
Text GLabel 12850 5900 2    60   Input ~ 0
COL5
Text GLabel 12850 6000 2    60   Input ~ 0
BA
Text GLabel 12850 6100 2    60   Input ~ 0
BB
Text GLabel 12850 6200 2    60   Input ~ 0
COL4
Text GLabel 12850 6500 2    60   Input ~ 0
COL1
Text GLabel 12850 6600 2    60   Input ~ 0
I2S_MCLK
Text GLabel 12850 6700 2    60   Input ~ 0
ROW2
Text GLabel 12850 6800 2    60   Input ~ 0
ROW1
Text GLabel 12850 6900 2    60   Input ~ 0
I2S_SCLK
Text GLabel 12850 7000 2    60   Input ~ 0
I2S_DOUT
Text GLabel 12850 7100 2    60   Input ~ 0
I2S_DIN
Text GLabel 12850 7200 2    60   Input ~ 0
B2
Text GLabel 12850 7300 2    60   Input ~ 0
B10
Text GLabel 12850 7400 2    60   Input ~ 0
B1
Text GLabel 12850 4100 2    60   Input ~ 0
Play
Text GLabel 12850 4200 2    60   Input ~ 0
B8
Text GLabel 12850 4300 2    60   Input ~ 0
Write
Text GLabel 12850 4400 2    60   Input ~ 0
B16
Text GLabel 12850 4600 2    60   Input ~ 0
USB_Over_Current
Text GLabel 12850 4700 2    60   Input ~ 0
B7
Text GLabel 12850 4800 2    60   Input ~ 0
B6
Text GLabel 12850 4900 2    60   Input ~ 0
BC
Text GLabel 12850 5000 2    60   Input ~ 0
BPM_VOL
Text GLabel 12850 5100 2    60   Input ~ 0
BE
Text GLabel 12850 5200 2    60   Input ~ 0
FX
Text GLabel 12850 2500 2    60   Input ~ 0
B13
Text GLabel 12850 2700 2    60   Input ~ 0
B3
Text GLabel 12850 2800 2    60   Input ~ 0
B12
Text GLabel 12850 2900 2    60   Input ~ 0
B11
Text GLabel 2850 5200 0    60   Input ~ 0
FS_DM
Text GLabel 2850 5300 0    60   Input ~ 0
FS_DP
$Comp
L GND #PWR016
U 1 1 585E3358
P 7650 7850
F 0 "#PWR016" H 7650 7850 30  0001 C CNN
F 1 "GND" H 7650 7780 30  0001 C CNN
F 2 "" H 7650 7850 60  0001 C CNN
F 3 "" H 7650 7850 60  0001 C CNN
	1    7650 7850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR017
U 1 1 585E336D
P 7850 7850
F 0 "#PWR017" H 7850 7850 30  0001 C CNN
F 1 "GND" H 7850 7780 30  0001 C CNN
F 2 "" H 7850 7850 60  0001 C CNN
F 3 "" H 7850 7850 60  0001 C CNN
	1    7850 7850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR018
U 1 1 585E337B
P 8050 7850
F 0 "#PWR018" H 8050 7850 30  0001 C CNN
F 1 "GND" H 8050 7780 30  0001 C CNN
F 2 "" H 8050 7850 60  0001 C CNN
F 3 "" H 8050 7850 60  0001 C CNN
	1    8050 7850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR019
U 1 1 585E3389
P 8250 7850
F 0 "#PWR019" H 8250 7850 30  0001 C CNN
F 1 "GND" H 8250 7780 30  0001 C CNN
F 2 "" H 8250 7850 60  0001 C CNN
F 3 "" H 8250 7850 60  0001 C CNN
	1    8250 7850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR020
U 1 1 585E3397
P 7450 7850
F 0 "#PWR020" H 7450 7850 30  0001 C CNN
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
L GND #PWR021
U 1 1 585E35DF
P 8750 8250
F 0 "#PWR021" H 8750 8250 30  0001 C CNN
F 1 "GND" H 8750 8180 30  0001 C CNN
F 2 "" H 8750 8250 60  0001 C CNN
F 3 "" H 8750 8250 60  0001 C CNN
	1    8750 8250
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR022
U 1 1 585E35F3
P 9250 8250
F 0 "#PWR022" H 9250 8250 30  0001 C CNN
F 1 "GND" H 9250 8180 30  0001 C CNN
F 2 "" H 9250 8250 60  0001 C CNN
F 3 "" H 9250 8250 60  0001 C CNN
	1    9250 8250
	1    0    0    -1  
$EndComp
Text GLabel 12850 6400 2    60   Input ~ 0
ROW4
$EndSCHEMATC
