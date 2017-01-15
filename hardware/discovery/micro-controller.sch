EESchema Schematic File Version 2
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
LIBS:wee_noise_maker
LIBS:wnm_discovery-cache
EELAYER 25 0
EELAYER END
$Descr A3 16535 11693
encoding utf-8
Sheet 2 3
Title "Wee Noise Maker - Mk-I"
Date ""
Rev "A"
Comp "Fabien Chouteau"
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
Text GLabel 8750 5950 2    60   Input ~ 0
VBUS
Text GLabel 8750 5800 2    60   Input ~ 0
SWDIO
Text GLabel 7800 5650 0    60   Input ~ 0
SWCLK
Text GLabel 7800 4450 0    60   Input ~ 0
Audio_SCL
Text GLabel 8750 4150 2    60   Input ~ 0
Audio_SDA
Text GLabel 8750 4750 2    60   Input ~ 0
I2S_SCLK
Text GLabel 7800 4600 0    60   Input ~ 0
I2S_DOUT
Text GLabel 8750 4600 2    60   Input ~ 0
I2S_DIN
$Comp
L GND #PWR07
U 1 1 585E35F3
P 5650 6450
F 0 "#PWR07" H 5650 6450 30  0001 C CNN
F 1 "GND" H 5650 6380 30  0001 C CNN
F 2 "" H 5650 6450 60  0001 C CNN
F 3 "" H 5650 6450 60  0001 C CNN
	1    5650 6450
	1    0    0    -1  
$EndComp
Text GLabel 5600 3550 2    60   Input ~ 0
WAKEUP
Text GLabel 7800 3100 0    60   Input ~ 0
3V3
Text GLabel 8750 5650 2    60   Input ~ 0
I2S_LRCLK
Text GLabel 8750 6250 2    60   Input ~ 0
I2S_MCLK
Text GLabel 7800 6100 0    60   Input ~ 0
SD_DAT0
Text GLabel 7800 6250 0    60   Input ~ 0
Play
Text GLabel 4650 6250 0    60   Input ~ 0
FX
Text GLabel 5600 6100 2    60   Input ~ 0
COL9
Text GLabel 4650 6100 0    60   Input ~ 0
ROW1
Text GLabel 5600 5950 2    60   Input ~ 0
ROW2
Text GLabel 4650 5950 0    60   Input ~ 0
ROW3
Text GLabel 5600 5800 2    60   Input ~ 0
B16
Text GLabel 4650 5800 0    60   Input ~ 0
B8
Text GLabel 5600 5650 2    60   Input ~ 0
BE
Text GLabel 4650 5650 0    60   Input ~ 0
COL8
Text GLabel 5600 5350 2    60   Input ~ 0
B15
Text GLabel 4650 5500 0    60   Input ~ 0
B7
Text GLabel 5600 5500 2    60   Input ~ 0
BD
Text GLabel 4650 5350 0    60   Input ~ 0
COL7
Text GLabel 5600 5200 2    60   Input ~ 0
B14
Text GLabel 4650 5200 0    60   Input ~ 0
B6
Text GLabel 5600 5050 2    60   Input ~ 0
BC
Text GLabel 4650 5050 0    60   Input ~ 0
COL6
Text GLabel 5600 4900 2    60   Input ~ 0
B13
Text GLabel 4650 4900 0    60   Input ~ 0
B5
Text GLabel 5600 4750 2    60   Input ~ 0
BB
Text GLabel 4650 4750 0    60   Input ~ 0
COL5
Text GLabel 8750 5500 2    60   Input ~ 0
SD_DAT3
Text GLabel 8750 6100 2    60   Input ~ 0
SD_DAT1
Text GLabel 7800 5500 0    60   Input ~ 0
SD_DAT2
Text GLabel 7800 5800 0    60   Input ~ 0
Write
Text GLabel 7800 5350 0    60   Input ~ 0
SD_CLK
Text GLabel 8750 5200 2    60   Input ~ 0
SD_CMD
Text GLabel 5600 4600 2    60   Input ~ 0
B12
Text GLabel 4650 4600 0    60   Input ~ 0
B4
Text GLabel 4650 4300 0    60   Input ~ 0
BA
Text GLabel 5600 4300 2    60   Input ~ 0
COL4
Text GLabel 4650 4150 0    60   Input ~ 0
B11
Text GLabel 5600 4150 2    60   Input ~ 0
B3
Text GLabel 5600 3850 2    60   Input ~ 0
COL3
Text GLabel 4650 3700 0    60   Input ~ 0
B10
Text GLabel 4650 3400 0    60   Input ~ 0
B9
Text GLabel 5600 3700 2    60   Input ~ 0
B2
Text GLabel 5600 3400 2    60   Input ~ 0
COL2
Text GLabel 4650 3250 0    60   Input ~ 0
B1
Text GLabel 5600 3250 2    60   Input ~ 0
COL1
Text GLabel 4650 3850 0    60   Input ~ 0
ENC_1_A
Text GLabel 4650 3550 0    60   Input ~ 0
ENC_1_B
Text GLabel 5600 4000 2    60   Input ~ 0
ENC_2_A
Text GLabel 4650 4000 0    60   Input ~ 0
ENC_2_B
Text GLabel 7800 3400 0    60   Input ~ 0
ENC_1_SW
Text GLabel 8750 3400 2    60   Input ~ 0
ENC_2_SW
$Comp
L STM32F4_DISCO_Header U1
U 1 1 5872D50A
P 6750 4350
F 0 "U1" H 4900 6950 60  0000 C CNN
F 1 "STM32F4_DISCO_Header" H 5300 6750 60  0000 C CNN
F 2 "POSS:STM32F4-DISCO-HEADERS-TH" H 6850 6450 60  0001 C CNN
F 3 "" H 6850 6450 60  0001 C CNN
	1    6750 4350
	1    0    0    -1  
$EndComp
Text GLabel 8750 3100 2    60   Input ~ 0
3V3
Wire Wire Line
	5600 6400 5650 6400
Wire Wire Line
	5650 6400 5650 6450
$Comp
L GND #PWR011
U 1 1 5873195F
P 4600 6450
F 0 "#PWR011" H 4600 6450 30  0001 C CNN
F 1 "GND" H 4600 6380 30  0001 C CNN
F 2 "" H 4600 6450 60  0001 C CNN
F 3 "" H 4600 6450 60  0001 C CNN
	1    4600 6450
	1    0    0    -1  
$EndComp
Wire Wire Line
	4650 6400 4600 6400
Wire Wire Line
	4600 6400 4600 6450
$Comp
L GND #PWR012
U 1 1 58731B7C
P 7750 6450
F 0 "#PWR012" H 7750 6450 30  0001 C CNN
F 1 "GND" H 7750 6380 30  0001 C CNN
F 2 "" H 7750 6450 60  0001 C CNN
F 3 "" H 7750 6450 60  0001 C CNN
	1    7750 6450
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR013
U 1 1 58731D0D
P 8800 6450
F 0 "#PWR013" H 8800 6450 30  0001 C CNN
F 1 "GND" H 8800 6380 30  0001 C CNN
F 2 "" H 8800 6450 60  0001 C CNN
F 3 "" H 8800 6450 60  0001 C CNN
	1    8800 6450
	1    0    0    -1  
$EndComp
Wire Wire Line
	8800 6450 8800 6400
Wire Wire Line
	8800 6400 8750 6400
Wire Wire Line
	7800 6400 7750 6400
Wire Wire Line
	7750 6400 7750 6450
$Comp
L GND #PWR014
U 1 1 58731F62
P 7700 2850
F 0 "#PWR014" H 7700 2850 30  0001 C CNN
F 1 "GND" H 7700 2780 30  0001 C CNN
F 2 "" H 7700 2850 60  0001 C CNN
F 3 "" H 7700 2850 60  0001 C CNN
	1    7700 2850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR015
U 1 1 5873205D
P 8800 2850
F 0 "#PWR015" H 8800 2850 30  0001 C CNN
F 1 "GND" H 8800 2780 30  0001 C CNN
F 2 "" H 8800 2850 60  0001 C CNN
F 3 "" H 8800 2850 60  0001 C CNN
	1    8800 2850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR016
U 1 1 587320C2
P 8800 2450
F 0 "#PWR016" H 8800 2450 30  0001 C CNN
F 1 "GND" H 8800 2380 30  0001 C CNN
F 2 "" H 8800 2450 60  0001 C CNN
F 3 "" H 8800 2450 60  0001 C CNN
	1    8800 2450
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR017
U 1 1 58732127
P 7750 2450
F 0 "#PWR017" H 7750 2450 30  0001 C CNN
F 1 "GND" H 7750 2380 30  0001 C CNN
F 2 "" H 7750 2450 60  0001 C CNN
F 3 "" H 7750 2450 60  0001 C CNN
	1    7750 2450
	1    0    0    -1  
$EndComp
Wire Wire Line
	7750 2450 7750 2400
Wire Wire Line
	7750 2400 7800 2400
Wire Wire Line
	8750 2400 8800 2400
Wire Wire Line
	8800 2400 8800 2450
Wire Wire Line
	8750 2800 8800 2800
Wire Wire Line
	8800 2800 8800 2850
Wire Wire Line
	7800 2800 7700 2800
Wire Wire Line
	7700 2800 7700 2850
$Comp
L GND #PWR018
U 1 1 58732836
P 5700 2850
F 0 "#PWR018" H 5700 2850 30  0001 C CNN
F 1 "GND" H 5700 2780 30  0001 C CNN
F 2 "" H 5700 2850 60  0001 C CNN
F 3 "" H 5700 2850 60  0001 C CNN
	1    5700 2850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR019
U 1 1 5873289B
P 5650 2450
F 0 "#PWR019" H 5650 2450 30  0001 C CNN
F 1 "GND" H 5650 2380 30  0001 C CNN
F 2 "" H 5650 2450 60  0001 C CNN
F 3 "" H 5650 2450 60  0001 C CNN
	1    5650 2450
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR020
U 1 1 58732964
P 4600 2450
F 0 "#PWR020" H 4600 2450 30  0001 C CNN
F 1 "GND" H 4600 2380 30  0001 C CNN
F 2 "" H 4600 2450 60  0001 C CNN
F 3 "" H 4600 2450 60  0001 C CNN
	1    4600 2450
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR021
U 1 1 587329C9
P 4600 2850
F 0 "#PWR021" H 4600 2850 30  0001 C CNN
F 1 "GND" H 4600 2780 30  0001 C CNN
F 2 "" H 4600 2850 60  0001 C CNN
F 3 "" H 4600 2850 60  0001 C CNN
	1    4600 2850
	1    0    0    -1  
$EndComp
Wire Wire Line
	4600 2850 4600 2800
Wire Wire Line
	4600 2800 4650 2800
Wire Wire Line
	4600 2450 4600 2400
Wire Wire Line
	4600 2400 4650 2400
Wire Wire Line
	5600 2400 5650 2400
Wire Wire Line
	5650 2400 5650 2450
Wire Wire Line
	5600 2800 5700 2800
Wire Wire Line
	5700 2800 5700 2850
$EndSCHEMATC
