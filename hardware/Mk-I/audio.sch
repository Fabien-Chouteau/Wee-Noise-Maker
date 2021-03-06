EESchema Schematic File Version 2
LIBS:wnm_mk_1-rescue
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
LIBS:msp430
LIBS:nxp_armmcu
LIBS:powerint
LIBS:pspice
LIBS:references
LIBS:relays
LIBS:sensors
LIBS:stm8
LIBS:stm32
LIBS:supertex
LIBS:transf
LIBS:ttl_ieee
LIBS:video
LIBS:wee_noise_maker
LIBS:wnm_mk_1-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 5 6
Title "Wee Noise Maker - Mk-I"
Date ""
Rev "D"
Comp "Fabien Chouteau"
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
Text GLabel 4500 3300 0    60   Input ~ 0
Audio_SDA
Text GLabel 4500 3450 0    60   Input ~ 0
Audio_SCL
Text GLabel 4500 3700 0    60   Input ~ 0
I2S_MCLK
Text GLabel 4500 3850 0    60   Input ~ 0
I2S_SCLK
Text GLabel 4500 4000 0    60   Input ~ 0
I2S_LRCLK
Text GLabel 4500 4150 0    60   Input ~ 0
I2S_DIN
Text GLabel 4500 4300 0    60   Input ~ 0
I2S_DOUT
Text GLabel 3600 2500 0    60   Input ~ 0
3V3
Wire Wire Line
	4500 4650 4450 4650
Wire Wire Line
	4450 4650 4450 4850
$Comp
L GND-RESCUE-wnm_mk_1 #PWR032
U 1 1 585E79AD
P 4450 4850
AR Path="/585E79AD" Ref="#PWR032"  Part="1" 
AR Path="/585E4C2D/585E79AD" Ref="#PWR029"  Part="1" 
F 0 "#PWR029" H 4450 4850 30  0001 C CNN
F 1 "GND" H 4450 4780 30  0001 C CNN
F 2 "" H 4450 4850 60  0001 C CNN
F 3 "" H 4450 4850 60  0001 C CNN
	1    4450 4850
	1    0    0    -1  
$EndComp
$Comp
L C-RESCUE-wnm_mk_1 C6
U 1 1 585E7B78
P 4100 4700
AR Path="/585E7B78" Ref="C6"  Part="1" 
AR Path="/585E4C2D/585E7B78" Ref="C6"  Part="1" 
F 0 "C6" H 4100 4800 40  0000 L CNN
F 1 "0.1uF" H 4106 4615 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 4138 4550 30  0001 C CNN
F 3 "" H 4100 4700 60  0000 C CNN
	1    4100 4700
	1    0    0    -1  
$EndComp
$Comp
L GND-RESCUE-wnm_mk_1 #PWR033
U 1 1 585E7BA3
P 4100 4900
AR Path="/585E7BA3" Ref="#PWR033"  Part="1" 
AR Path="/585E4C2D/585E7BA3" Ref="#PWR030"  Part="1" 
F 0 "#PWR030" H 4100 4900 30  0001 C CNN
F 1 "GND" H 4100 4830 30  0001 C CNN
F 2 "" H 4100 4900 60  0001 C CNN
F 3 "" H 4100 4900 60  0001 C CNN
	1    4100 4900
	1    0    0    -1  
$EndComp
Wire Wire Line
	4500 4500 4100 4500
Wire Wire Line
	5750 3900 6725 3900
Wire Wire Line
	5750 4050 6725 4050
$Comp
L C-RESCUE-wnm_mk_1 C7
U 1 1 585E8504
P 3800 2750
AR Path="/585E8504" Ref="C7"  Part="1" 
AR Path="/585E4C2D/585E8504" Ref="C7"  Part="1" 
F 0 "C7" H 3800 2850 40  0000 L CNN
F 1 "0.1uF" H 3806 2665 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 3838 2600 30  0001 C CNN
F 3 "" H 3800 2750 60  0000 C CNN
	1    3800 2750
	1    0    0    -1  
$EndComp
$Comp
L C-RESCUE-wnm_mk_1 C8
U 1 1 585E857A
P 4050 2750
AR Path="/585E857A" Ref="C8"  Part="1" 
AR Path="/585E4C2D/585E857A" Ref="C8"  Part="1" 
F 0 "C8" H 4050 2850 40  0000 L CNN
F 1 "0.1uF" H 4056 2665 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 4088 2600 30  0001 C CNN
F 3 "" H 4050 2750 60  0000 C CNN
	1    4050 2750
	1    0    0    -1  
$EndComp
$Comp
L C-RESCUE-wnm_mk_1 C9
U 1 1 585E859F
P 5100 2050
AR Path="/585E859F" Ref="C9"  Part="1" 
AR Path="/585E4C2D/585E859F" Ref="C9"  Part="1" 
F 0 "C9" H 5100 2150 40  0000 L CNN
F 1 "0.1uF" H 5106 1965 40  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 5138 1900 30  0001 C CNN
F 3 "" H 5100 2050 60  0000 C CNN
	1    5100 2050
	1    0    0    -1  
$EndComp
$Comp
L SGTL5000 U5
U 1 1 585E883F
P 4750 3200
F 0 "U5" H 4650 3450 60  0000 C CNN
F 1 "SGTL5000XNAA3R2" H 5100 1650 60  0000 C CNN
F 2 "Housings_DFN_QFN:UQFN-20-1EP_3x3mm_Pitch0.4mm" H 4750 3150 60  0001 C CNN
F 3 "" H 4750 3150 60  0001 C CNN
	1    4750 3200
	1    0    0    -1  
$EndComp
Wire Wire Line
	3800 2550 3800 2500
Connection ~ 3800 2500
Wire Wire Line
	4050 2500 4050 2550
Connection ~ 4050 2500
$Comp
L Csmall C10
U 1 1 585E8C6B
P 6000 3300
F 0 "C10" V 6050 3200 30  0000 L CNN
F 1 "1uF" V 6050 3350 30  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 6000 3300 60  0001 C CNN
F 3 "" H 6000 3300 60  0000 C CNN
	1    6000 3300
	0    1    1    0   
$EndComp
$Comp
L Csmall C11
U 1 1 585E8E3F
P 6000 3450
F 0 "C11" V 6050 3350 30  0000 L CNN
F 1 "1uF" V 6050 3500 30  0000 L CNN
F 2 "Capacitors_SMD:C_0805" H 6000 3450 60  0001 C CNN
F 3 "" H 6000 3450 60  0000 C CNN
	1    6000 3450
	0    1    1    0   
$EndComp
Wire Wire Line
	5900 3300 5750 3300
Wire Wire Line
	5900 3450 5750 3450
Wire Wire Line
	6100 3300 6725 3300
Wire Wire Line
	6100 3450 6725 3450
$Comp
L 3.5_audio_jack HP_OUT1
U 1 1 5863AC9E
P 7100 4425
F 0 "HP_OUT1" H 7175 3800 60  0000 C CNN
F 1 "SJ-3523-SMT" H 7100 4425 60  0001 C CNN
F 2 "POSS:SJ-3523-SMT" H 7100 4425 60  0001 C CNN
F 3 "" H 7100 4425 60  0001 C CNN
	1    7100 4425
	-1   0    0    1   
$EndComp
Wire Wire Line
	5750 4200 6725 4200
$Comp
L 3.5_audio_jack LINE_IN1
U 1 1 5863B129
P 7100 3825
F 0 "LINE_IN1" H 7175 3200 60  0000 C CNN
F 1 "SJ-3523-SMT" H 6950 3200 60  0001 C CNN
F 2 "POSS:SJ-3523-SMT" H 7100 3825 60  0001 C CNN
F 3 "" H 7100 3825 60  0001 C CNN
	1    7100 3825
	-1   0    0    1   
$EndComp
$Comp
L GND-RESCUE-wnm_mk_1 #PWR034
U 1 1 5863B3F7
P 6675 3675
AR Path="/5863B3F7" Ref="#PWR034"  Part="1" 
AR Path="/585E4C2D/5863B3F7" Ref="#PWR031"  Part="1" 
F 0 "#PWR031" H 6675 3675 30  0001 C CNN
F 1 "GND" H 6675 3605 30  0001 C CNN
F 2 "" H 6675 3675 60  0001 C CNN
F 3 "" H 6675 3675 60  0001 C CNN
	1    6675 3675
	1    0    0    -1  
$EndComp
Wire Wire Line
	6725 3600 6675 3600
Wire Wire Line
	6675 3600 6675 3675
Wire Wire Line
	3600 2500 4900 2500
$Comp
L GND-RESCUE-wnm_mk_1 #PWR032
U 1 1 58BC5E4F
P 4050 3000
F 0 "#PWR032" H 4050 3000 30  0001 C CNN
F 1 "GND" H 4050 2930 30  0001 C CNN
F 2 "" H 4050 3000 60  0001 C CNN
F 3 "" H 4050 3000 60  0001 C CNN
	1    4050 3000
	1    0    0    -1  
$EndComp
$Comp
L APE8865N-18-HF-3 U1
U 1 1 58E3EC1A
P 4500 1900
F 0 "U1" H 4200 2150 50  0000 C CNN
F 1 "AP7313-18SRG-7" H 4500 2100 50  0000 C CNN
F 2 "TO_SOT_Packages_SMD:SOT-23" H 4500 2000 50  0000 C CIN
F 3 "" H 4500 1900 50  0000 C CNN
	1    4500 1900
	1    0    0    -1  
$EndComp
Wire Wire Line
	4900 2900 5150 2900
Wire Wire Line
	4900 2500 4900 2900
Wire Wire Line
	3800 2950 4050 2950
Wire Wire Line
	4050 2950 4050 3000
Text GLabel 3950 1850 0    60   Input ~ 0
3V3
Wire Wire Line
	3950 1850 4050 1850
Wire Wire Line
	4950 1850 5400 1850
Wire Wire Line
	5400 1850 5400 2900
Connection ~ 5100 1850
Wire Wire Line
	5100 2250 4500 2250
Wire Wire Line
	4500 2250 4500 2200
$Comp
L GND-RESCUE-wnm_mk_1 #PWR033
U 1 1 58E3F7FA
P 4800 2300
F 0 "#PWR033" H 4800 2300 30  0001 C CNN
F 1 "GND" H 4800 2230 30  0001 C CNN
F 2 "" H 4800 2300 60  0001 C CNN
F 3 "" H 4800 2300 60  0001 C CNN
	1    4800 2300
	1    0    0    -1  
$EndComp
Wire Wire Line
	4800 2300 4800 2250
Connection ~ 4800 2250
$Comp
L TEST TP_MCLK1
U 1 1 59ECF1C0
P 2200 5700
F 0 "TP_MCLK1" V 2200 6200 50  0000 C BNN
F 1 "TEST" H 2200 5950 50  0000 C CNN
F 2 "Measurement_Points:Measurement_Point_Round-SMD-Pad_Small" H 2200 5700 50  0001 C CNN
F 3 "" H 2200 5700 50  0001 C CNN
	1    2200 5700
	0    1    1    0   
$EndComp
Text GLabel 2200 5700 0    60   Input ~ 0
I2S_MCLK
Text GLabel 2200 5450 0    60   Input ~ 0
I2S_SCLK
Text GLabel 2200 5200 0    60   Input ~ 0
I2S_LRCLK
Text GLabel 2200 4950 0    60   Input ~ 0
I2S_DIN
Text GLabel 2200 4700 0    60   Input ~ 0
I2S_DOUT
$Comp
L TEST TP_SCLK1
U 1 1 59ECF440
P 2200 5450
F 0 "TP_SCLK1" V 2200 5950 50  0000 C BNN
F 1 "TEST" H 2200 5700 50  0000 C CNN
F 2 "Measurement_Points:Measurement_Point_Round-SMD-Pad_Small" H 2200 5450 50  0001 C CNN
F 3 "" H 2200 5450 50  0001 C CNN
	1    2200 5450
	0    1    1    0   
$EndComp
$Comp
L TEST TP_LRCLK1
U 1 1 59ECF47A
P 2200 5200
F 0 "TP_LRCLK1" V 2200 5750 50  0000 C BNN
F 1 "TEST" H 2200 5450 50  0000 C CNN
F 2 "Measurement_Points:Measurement_Point_Round-SMD-Pad_Small" H 2200 5200 50  0001 C CNN
F 3 "" H 2200 5200 50  0001 C CNN
	1    2200 5200
	0    1    1    0   
$EndComp
$Comp
L TEST TP_DIN1
U 1 1 59ECF4B7
P 2200 4950
F 0 "TP_DIN1" V 2200 5450 50  0000 C BNN
F 1 "TEST" H 2200 5200 50  0000 C CNN
F 2 "Measurement_Points:Measurement_Point_Round-SMD-Pad_Small" H 2200 4950 50  0001 C CNN
F 3 "" H 2200 4950 50  0001 C CNN
	1    2200 4950
	0    1    1    0   
$EndComp
$Comp
L TEST TP_DOUT1
U 1 1 59ECF4F7
P 2200 4700
F 0 "TP_DOUT1" V 2200 5250 50  0000 C BNN
F 1 "TEST" H 2200 4950 50  0000 C CNN
F 2 "Measurement_Points:Measurement_Point_Round-SMD-Pad_Small" H 2200 4700 50  0001 C CNN
F 3 "" H 2200 4700 50  0001 C CNN
	1    2200 4700
	0    1    1    0   
$EndComp
Text GLabel 2200 4350 0    60   Input ~ 0
Audio_SCL
Text GLabel 2200 4100 0    60   Input ~ 0
Audio_SDA
$Comp
L TEST TP_SCL1
U 1 1 59ECF77C
P 2200 4350
F 0 "TP_SCL1" V 2200 4850 50  0000 C BNN
F 1 "TEST" H 2200 4600 50  0000 C CNN
F 2 "Measurement_Points:Measurement_Point_Round-SMD-Pad_Small" H 2200 4350 50  0001 C CNN
F 3 "" H 2200 4350 50  0001 C CNN
	1    2200 4350
	0    1    1    0   
$EndComp
$Comp
L TEST TP_SDA1
U 1 1 59ECF7C8
P 2200 4100
F 0 "TP_SDA1" V 2200 4600 50  0000 C BNN
F 1 "TEST" H 2200 4350 50  0000 C CNN
F 2 "Measurement_Points:Measurement_Point_Round-SMD-Pad_Small" H 2200 4100 50  0001 C CNN
F 3 "" H 2200 4100 50  0001 C CNN
	1    2200 4100
	0    1    1    0   
$EndComp
$EndSCHEMATC
