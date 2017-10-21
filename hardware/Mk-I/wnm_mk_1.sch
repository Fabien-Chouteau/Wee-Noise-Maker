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
Sheet 1 6
Title "Wee Noise Maker - Mk-I"
Date ""
Rev "C"
Comp "Fabien Chouteau"
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Sheet
S 750  650  1350 1100
U 585D2CBE
F0 "Power" 60
F1 "power.sch" 60
$EndSheet
$Sheet
S 2400 650  1400 1100
U 585DBE08
F0 "Micro-Controller" 60
F1 "micro-controller.sch" 60
$EndSheet
$Sheet
S 4050 650  1450 1050
U 585D6FEE
F0 "LED_Matrix" 60
F1 "led_matrix.sch" 60
$EndSheet
$Sheet
S 5750 650  1350 1050
U 585E4C2D
F0 "Audio" 60
F1 "audio.sch" 60
$EndSheet
Text Notes 4250 6200 0    79   ~ 0
TODO:\n - Different SD card slot\n - Change button layout and labels (smaller distance between button rows?)\n - Encoders filtering?\n - Different screen (bigger, e-paper?)\n - Fold screen connector under the screen\n - resistor network for the sd card\n - Wake up circuit\n - remove bottom-right hole\n - Add test points (I2S, SD card, I2C)\n - Lower the number of different values of capas and resistors\n\nDONE:\n - I2C extension port\n - SMD crystal\n - SMD battery connector\n
Text Notes 650  5200 0    79   ~ 0
Known problems:\n\nFixed:\n - DAC 1.8V VDDD\n - DAC couplings caps are in series with the chip \n   instead of going to GND\n - Voltage regulator footprint\n - Encoders footprints and hole size\n
$Sheet
S 7350 650  1350 1050
U 59EC5AE7
F0 "Button Matrix" 60
F1 "button_matrix.sch" 60
$EndSheet
$EndSCHEMATC
