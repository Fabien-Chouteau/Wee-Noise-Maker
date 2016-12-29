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
$Descr A4 11693 8268
encoding utf-8
Sheet 5 5
Title "Pocket Open Source Syntesizer - Discovery"
Date ""
Rev "B"
Comp "Fabien Chouteau"
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
Text GLabel 5150 2900 0    60   Input ~ 0
Audio_SDA
Text GLabel 5150 3050 0    60   Input ~ 0
Audio_SCL
Text GLabel 5150 3300 0    60   Input ~ 0
I2S_MCLK
Text GLabel 5150 3450 0    60   Input ~ 0
I2S_SCLK
Text GLabel 5150 3600 0    60   Input ~ 0
I2S_LRCLK
Text GLabel 5150 3750 0    60   Input ~ 0
I2S_DIN
Text GLabel 5150 3900 0    60   Input ~ 0
I2S_DOUT
Text GLabel 5250 2050 0    60   Input ~ 0
3V3
Wire Wire Line
	5150 4250 5100 4250
Wire Wire Line
	5100 4250 5100 4450
$Comp
L GND #PWR028
U 1 1 585E79AD
P 5100 4450
F 0 "#PWR028" H 5100 4450 30  0001 C CNN
F 1 "GND" H 5100 4380 30  0001 C CNN
F 2 "" H 5100 4450 60  0001 C CNN
F 3 "" H 5100 4450 60  0001 C CNN
	1    5100 4450
	1    0    0    -1  
$EndComp
$Comp
L C C6
U 1 1 585E7B78
P 4750 4300
F 0 "C6" H 4750 4400 40  0000 L CNN
F 1 "0.1uF" H 4756 4215 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 4788 4150 30  0001 C CNN
F 3 "" H 4750 4300 60  0000 C CNN
	1    4750 4300
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR029
U 1 1 585E7BA3
P 4750 4500
F 0 "#PWR029" H 4750 4500 30  0001 C CNN
F 1 "GND" H 4750 4430 30  0001 C CNN
F 2 "" H 4750 4500 60  0001 C CNN
F 3 "" H 4750 4500 60  0001 C CNN
	1    4750 4500
	1    0    0    -1  
$EndComp
Wire Wire Line
	5150 4100 4750 4100
Wire Wire Line
	6400 3500 7375 3500
Wire Wire Line
	6400 3650 7375 3650
$Comp
L C C7
U 1 1 585E8504
P 5550 2300
F 0 "C7" H 5550 2400 40  0000 L CNN
F 1 "0.1uF" H 5556 2215 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 5588 2150 30  0001 C CNN
F 3 "" H 5550 2300 60  0000 C CNN
	1    5550 2300
	1    0    0    -1  
$EndComp
$Comp
L C C8
U 1 1 585E857A
P 5800 2300
F 0 "C8" H 5800 2400 40  0000 L CNN
F 1 "0.1uF" H 5806 2215 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 5838 2150 30  0001 C CNN
F 3 "" H 5800 2300 60  0000 C CNN
	1    5800 2300
	1    0    0    -1  
$EndComp
$Comp
L C C9
U 1 1 585E859F
P 6050 2300
F 0 "C9" H 6050 2400 40  0000 L CNN
F 1 "0.1uF" H 6056 2215 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 6088 2150 30  0001 C CNN
F 3 "" H 6050 2300 60  0000 C CNN
	1    6050 2300
	1    0    0    -1  
$EndComp
$Comp
L SGTL5000 U5
U 1 1 585E883F
P 5400 2800
F 0 "U5" H 5300 3050 60  0000 C CNN
F 1 "SGTL5000" H 5750 1250 60  0000 C CNN
F 2 "Housings_DFN_QFN:UQFN-20-1EP_3x3mm_Pitch0.4mm" H 5400 2750 60  0001 C CNN
F 3 "" H 5400 2750 60  0001 C CNN
	1    5400 2800
	1    0    0    -1  
$EndComp
Wire Wire Line
	6050 2100 6050 2050
Wire Wire Line
	6050 2050 5250 2050
Wire Wire Line
	5550 2100 5550 2050
Connection ~ 5550 2050
Wire Wire Line
	5800 2100 5800 2050
Connection ~ 5800 2050
$Comp
L Csmall C10
U 1 1 585E8C6B
P 6650 2900
F 0 "C10" V 6700 2800 30  0000 L CNN
F 1 "1uF" V 6700 2950 30  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 6650 2900 60  0001 C CNN
F 3 "" H 6650 2900 60  0000 C CNN
	1    6650 2900
	0    1    1    0   
$EndComp
$Comp
L Csmall C11
U 1 1 585E8E3F
P 6650 3050
F 0 "C11" V 6700 2950 30  0000 L CNN
F 1 "1uF" V 6700 3100 30  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 6650 3050 60  0001 C CNN
F 3 "" H 6650 3050 60  0000 C CNN
	1    6650 3050
	0    1    1    0   
$EndComp
Wire Wire Line
	6550 2900 6400 2900
Wire Wire Line
	6550 3050 6400 3050
Wire Wire Line
	6750 2900 7375 2900
Wire Wire Line
	6750 3050 7375 3050
$Comp
L 3.5_audio_jack HP_OUT1
U 1 1 5863AC9E
P 7750 4025
F 0 "HP_OUT1" H 7825 3400 60  0000 C CNN
F 1 "3.5_audio_jack" H 7750 4025 60  0001 C CNN
F 2 "stm32f4-disco:lumberg_3.5_audio_jack_1503_02" H 7750 4025 60  0001 C CNN
F 3 "" H 7750 4025 60  0001 C CNN
	1    7750 4025
	-1   0    0    1   
$EndComp
Wire Wire Line
	6400 3800 7375 3800
$Comp
L 3.5_audio_jack LINE_IN1
U 1 1 5863B129
P 7750 3425
F 0 "LINE_IN1" H 7825 2800 60  0000 C CNN
F 1 "3.5_audio_jack" H 7600 2800 60  0001 C CNN
F 2 "stm32f4-disco:lumberg_3.5_audio_jack_1503_02" H 7750 3425 60  0001 C CNN
F 3 "" H 7750 3425 60  0001 C CNN
	1    7750 3425
	-1   0    0    1   
$EndComp
$Comp
L GND #PWR030
U 1 1 5863B3F7
P 7325 3275
F 0 "#PWR030" H 7325 3275 30  0001 C CNN
F 1 "GND" H 7325 3205 30  0001 C CNN
F 2 "" H 7325 3275 60  0001 C CNN
F 3 "" H 7325 3275 60  0001 C CNN
	1    7325 3275
	1    0    0    -1  
$EndComp
Wire Wire Line
	7375 3200 7325 3200
Wire Wire Line
	7325 3200 7325 3275
$EndSCHEMATC
