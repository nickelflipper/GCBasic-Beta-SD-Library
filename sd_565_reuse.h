'    SD Card routines for the GCBASIC compiler
'    Copyright (C) 2017 Kent Schafer

'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.

'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.

'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

'Notes:
'

'Changes:
' 8/16/2017 Fix FAT16 and FAT32 RootDIRAddr and FileDataAddr offsets
'           Add multi sector RootDIR capability
'           AND, Numerous Debug fixes  KSchafer
'8/25/2017  Major Housekeeping and renaming conventions
'           Reconcile offset differences for FAT16 2GB sdhc card
'           Lower SPI speed for sdsc V1 card after initialization
'           Add dummy SPITransfer for timing purposes for internal
'             registers OCR, CID, SCR
'           Merge GLCDDrawBMP into glcd.h library
'           Merge SD_ReadBMP into sd library  KSchafer

#startup Init_SD

Dim SDarg(512)
Dim Data(512)
'Dim FatFileName(8)
Dim StartAddr, OffsetAddr, BSEBlkAddr, RootDIRAddr, MoreDIRSectors, SizeFAT, FileDataAddr, FatFileLoc, FatFileSize as Long
Dim arg, argx, Try, sectorcount as Word


Sub Init_SD

'*************************************************
'*SD card Boot Block, Root Directory entries, and*
'*internal registers are little Endian format    *
'*************************************************

'set pin directions
dir SD_MISO In
dir SD_MOSI Out
dir SD_SCK Out
dir SD_CS Out

#define BootBlkAddr 0x00
'assumes 512 Bytes per sector
#define BytesPerSector 0x200
'assumes 512 entries * 32 Bytes
#define MaxRootEntries 0x4000

wait 100 ms   'stabilize voltage?

SD_MBR = False
SD_BSE = False
SD_SDHC = False
SD_ACMD41 = False
Fat12 = False
Fat16 = False
Fat32 = False
BMPAddr = False
LastRecord = False
LastEEAddr = 7
SD_NumFiles = 0

'SD card SPI bit rate must be =< 400kbs during initialization
'SPIMode (MasterSlow)    '1/64 of chipMHz
'SPIMode ( MasterSlow,SPI_CPOL_0+SPI_CPHA_0)

'*********************************************
'Reset  Sequence SPI Operation Mode
'valid commands
'CMD0   RESET
'CMD8   SEND_IF_COMD  'Must have correct CRC
'CMD41  SD_SEND_OP_COND
'CMD58  READ_OCR
'CMD59  CRC_ON_OFF  'CRC_OFF default in SPI
'*********************************************

'CMD0 Reset sequence
'Initialize SD card in SPI Mode
Call SD_Reset

'CMD8 SEND_IF_COND Sequence
'determine if card is V1 or V2
Call SD_Send_IF_COND

'CMD58 Read_OCR
'check Vdd range desired by host
Call SD_Read_OCR

'CMD ACMD41 SD_SEND_OP_COND
'start init and check init complete
'R1 response "1" still initializing
'"0" initialization complete
Call SD_SEND_OP_COND

'CMD58 Read_OCR
'SD card type only valid after ACMD41
Call SD_Read_OCR

'OSCCON = 112
'Set PLLEN ON

'up the SPI speed after initialization
'SDSC V1 seems to like slower SPI speed
  iF SDphysicalV2 = True Then
    SPIMode (MasterFast,SPI_CPOL_0+SPI_CPHA_0)
  Else
    SPIMode (Master,SPI_CPOL_0+SPI_CPHA_0)
  End If


'CMD10 SEND_CID
'send card identification information
  #IFDEF Debug_SD_CID

    Call SD_Read_CID

  #ENDIF


    ''CMD9 SEND_CSD_Structure
    ''send card specific data structure
    '  #IFDEF Debug_SD_CSD
    '
    '    Call SD_Read_CSD
    '
    '  #ENDIF

'CMD ACMD51 SEND_SCR
'Reads the SD Configuration Register
  #IFDEF Debug_SD_SCR

    Call SD_Read_SCR

  #ENDIF


''up the SPI speed after initialization
''SDSC V1 seems to like slower SPI speed
'  iF SDphysicalV2 = True Then
'    SPIMode (MasterFast,SPI_CPOL_0+SPI_CPHA_0)
'  Else
'    SPIMode (Master,SPI_CPOL_0+SPI_CPHA_0)
'  End If

'Reading internal registers complete
'Start reading sd card MBR, BSE, and Root Directory entries

  Call SD_ReadBlock (BootBlkAddr)

  If SD_MBR = True Then

    If FAT32 = True OR SD_SDHC = True Then
      Call SD_ReadBlock (BSEBlkAddr)
    Else
      'SDSC cards are Byte based Offsets
      BSEBlkAddr = BSEBlkAddr * 512
      Call SD_ReadBlock (BSEBlkAddr)
    End If

  End If

  Call SD_ReadBlock (RootDirAddr)

  'add more directory blocks as required
  MoreDIRSectors = RootDirAddr
  Do Until LastRecord = True
    'preserve First RootDIRAddr for FileDataAddr calc's
    MoreDIRSectors += 1
    Call SD_ReadBlock (MoreDIRSectors)
  Loop

  #IFDEF Debug_SD_Init

    HSerPrintCRLF
    HSerPrintCRLF
    If FAT12 = True Then HSerPrint "FAT12"
    If FAT16 = True Then HSErPrint "FAT16"
    If FAT32 = True Then HSerPrint "FAT32"
    HSerPrint " "

    If SD_BSE = False Then
      HSerPrint "Bad BSE address or Unsupported File System"
    End If

  #ENDIF

end sub


sub SD_Reset

'**********************************
'CMD0 Reset sequence
'Initialize SD card in SPI Mode
'**********************************
  #IfDef Debug_SD_Init
      HSerPrintCRLF
      HSerPrintCRLF
      HSerPrintCRLF
      HSerPrint "Reset SD Card..."
  #EndIf


  For Try = 1 to 25
    'must disassert SD card here
    Set SD_CS On
    'wait 1 ms
    'send 80 clock cycles before issuing commands
    Repeat 10
      SDxfer = 0xFF
      SPI_Xfer (SDxfer)
    end repeat

'    Repeat 10
'      SPITransfer (0xFF, dummy)
'    end repeat

    Call SD_SendCMDR1 (0x40, 0x00, 0x00, 0x00, 0x00, 0x95)
'    'now assert SD
'    Set SD_CS Off
'    'delay before sending command
'    SPITransfer (0xFF, dummy)
'    'set to SPI mode
'    SPITransfer (0x40, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x95, dummy)
'
'    'spin wheels waiting for SD response
'    repeat 5 '0
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 1 Then exit Repeat
'    end repeat
'    If SDresponse = 1 Then exit For
  Next

  #IfDef Debug_SD_Init
    If SDxfer <> 1 Then
    'If SDresponse <> 1 Then
      HSerPrint "SPI Mode Fail"
    Else
      HSerPrint "SPI Mode Ready"
    End If
    HSerPrintCRLF
  #EndIf

  Set SD_CS ON

end sub


sub SD_Send_IF_COND

'********************
'CMD8 SEND_IF_COND Sequence
'********************
arg1 = 0
arg2 = 0
arg3 = 1
arg4 = 170

  #IFDEF Debug_SD_Init
    HSerPrint "Check Spec Version..."
  #ENDIF

Call SD_SendCMDR1 (0x48, 0x00, 0x00, 0x01, 0xAA, 0x87)

'  Set SD_CS Off
'
'  For TryAgain = 1 to 50
'    'delay before sending command
'    SPITransfer (0xFF, dummy)
'    'CMD8 Send_IF_COND
'    'SD V1.x spec does not recognize CMD8
'    'SD V2.0 and up supports CMD8
'    SPITransfer (0x48, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x01, dummy)
'    SPITransfer (0xAA, dummy)
'    SPITransfer (0x87, dummy)
'
'    repeat 5
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 1 OR SDresponse = 5 Then exit Repeat
'    end repeat
'
'    If SDresponse = 1 OR SDresponse = 5 Then exit For
'
'  Next

  #IFDEF Debug_SD_Init
    HSerPrint "(":HSerPrint SDxfer:HSerPrint ")"
    'HSerPrint "(":HSerPrint SDresponse:HSerPrint ")"
  #ENDIF

  If SDxfer = 1 Then    'Idle state, valid voltage
  'If SDresponse = 1 Then    'Idle state, valid voltage

    pass = False

    For arg = 1 to 4
      SDxfer = 0xFF
      SPI_Xfer (SDxfer)
      'SPITransfer (0xFF, SDresponse)
      'SDarg(arg) = SDresponse
      SDarg(arg) = SDxfer
    Next

    If SDarg(1) = arg1 Then
      If SDarg(2) = arg2 Then
        If SDarg(3) = arg3 Then
          If SDarg(4) = arg4 Then
            pass = True
          End If
        End If
      End If
    End If

    If pass = True Then

      SDphysicalV2 = True
      #IFDEF Debug_SD_Init
        HSerPrint "SD V2.0+, SDSC, SDHC, or SDXC Card"
        HSerSend 13
        HSerSend 10
      #ENDIF

    End If

  End If

  'Verify valid R1 response in SPI mode 0x05 or 0x09?
  If SDxfer = 5 Then   'Illegal CMD error
  'If SDresponse = 5 Then   'Illegal CMD error
    SDphysicalV2 = False

    #IFDEF Debug_SD_Init
      HSerPrint "SD V1, or MMC Card "
      HSerPrintCRLF
    #ENDIF

  End If

  If SDxfer = 255 Then
  'If SDresponse = 255 Then
    SDphysicalV2 = False

    #IFDEF Debug_SD_Init
      HSerPrint "Unknown Card "
      HSerPrintCRLF
    #ENDIF

  End If

  Set SD_CS On

end sub


sub SD_Send_OP_COND

'**********************************
'CMD ACMD41 SD_SEND_OP_COND
'start init and check init complete
'R1 response "1" still initializing
'"0" initialization complete
'**********************************

'ACMD41 = CMD55 plus CMD41

  #IFDEF Debug_SD_Init
    HSerPrint "Init SPI Mode..."
  #ENDIF


  Set SD_CS Off

  'Can take several hundred ms to complete
  For TryInit = 1 to 255  '25

  Call SD_SendCMDR1 (0x77, 0x00, 0x00, 0x00, 0x00, 0xFF)

'    'delay before sending command
'    SPITransfer (0xFF, dummy)
'    'CMD55
'    SPITransfer (0x77, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0xFF, dummy)
'
'    'wait for response
'    repeat 10
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 1 Then exit Repeat
'    end repeat

    'delay before sending command
    'SPITransfer (0xFF, dummy)
    'CMD41
    'If CMD8 response false then V1 card
    'SD_ACMD41 = True

    If  SDphysicalV2 = True Then
      'Call SD_SendCMDR1 (0x69, 0x40, 0x00, 0x00, 0x00, 0xFF)

      SDxfer = 0x69
      SPI_Xfer (SDxfer)
      SDxfer = 0x40
      SPI_Xfer (SDxfer)
      SDxfer = 0x00
      SPI_Xfer (SDxfer)
      SDxfer = 0x00
      SPI_Xfer (SDxfer)
      SDxfer = 0x00
      SPI_Xfer (SDxfer)
      SDxfer = 0xFF
      SPI_Xfer (SDxfer)

'      SPITransfer (0x69, dummy)
'      SPITransfer (0x40, dummy)
'      SPITransfer (0x00, dummy)
'      SPITransfer (0x00, dummy)
'      SPITransfer (0x00, dummy)
'      SPITransfer (0xFF, dummy)
    Else    'V1 card send with HCS = 0
      'Call SD_SendCMDR1 (0x69, 0x00, 0x00, 0x00, 0x00, 0xFF)

      SDxfer = 0x69
      SPI_Xfer (SDxfer)
      SDxfer = 0x00
      SPI_Xfer (SDxfer)
      SDxfer = 0x00
      SPI_Xfer (SDxfer)
      SDxfer = 0x00
      SPI_Xfer (SDxfer)
      SDxfer = 0x00
      SPI_Xfer (SDxfer)
      SDxfer = 0xFF
      SPI_Xfer (SDxfer)

'      SPITransfer (0x69, dummy)
'      SPITransfer (0x00, dummy)
'      SPITransfer (0x00, dummy)
'      SPITransfer (0x00, dummy)
'      SPITransfer (0x00, dummy)
'      SPITransfer (0xFF, dummy)
    End If

    'wait for response
    repeat 10
      'wait 1 ms
      SDxfer = 0xFF
      SPI_Xfer (SDxfer)
      If SDxfer = 0 Then exit Repeat
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 0 Then exit Repeat
      wait 1 ms
    end repeat

    If SDxfer = 0 Then exit For
    'If SDresponse = 0 Then exit For

  Next

  #IFDEF Debug_SD_Init
    HSerPrint "(":HSerPrint SDxfer:HSerPrint ")"
    'HSerPrint "(":HSerPrint SDresponse:HSerPrint ")"

    If SDxfer = 0 Then
    'If SDresponse = 0 Then
      HSerPrint "SD Card Initialization Complete"
    Else
      HSerPrint "MMC or Incompatible Card"
    End If
    HSerPrintCRLF

  #ENDIF

  Set SD_CS On

  SD_ACMD41 = True

end sub


sub SD_Read_OCR
'********************
'CMD58 Read_OCR, check Vdd range desired by host
'********************

#IFDEF Debug_SD_OCR

  If SD_ACMD41 = False Then
    HSerPrint "Check VDD, CS, CCS..."
  Else
      HSerPrintCRLF
      HSerPrint "***Card Operating Config Register (OCR)***":HSerPrintCRLF
      HSerPrint "ReCheck VDD, CS, CCS, after init..."
  End If

#ENDIF

  Call SD_SendCMDR1 (0x7A, 0x00, 0x00, 0x00, 0xAA, 0xFF)
'  Set SD_CS Off
'
'  For TryAgain = 1 to 5
'
'    'delay before sending command
'    SPITransfer (0xFF, dummy)
'    'CMD8 Send_IF_COND
'    'SD V1.x spec does not recognize CMD8
'    'SD V2.0 and up supports CMD8
'    SPITransfer (0x7A, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0xAA, dummy)
'    SPITransfer (0xFF, dummy)
'
'    'wait till Card power up status bit is set
'    repeat 5
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 0 OR SDresponse = 1 Then exit Repeat    'Not in idle mode anymore?
'    end repeat
'
'    If SDresponse = 0 OR SDresponse = 1 Then exit For
'
'  Next

  #IFDEF Debug_SD_OCR
    HSerPrint "(":HSerPrint SDxfer:HSerPrint ")"
    'HSerPrint "(":HSerPrint SDresponse:HSerPrint ")"
  #ENDIF

  If SDxfer = 0 OR SDxfer = 1 Then    'Idle state, valid voltage
  'If SDresponse = 0 OR SDresponse = 1 Then    'Idle state, valid voltage

    'extract next four response bytes
    For arg = 1 to 4
      SDxfer = 0xFF
      SPI_Xfer (SDxfer)
      'SPITransfer (0xFF, SDresponse)
      'SDarg(arg) = SDresponse
      SDarg(arg) = SDxfer
    Next

    SDxfer = 0xFF
    SPI_Xfer (SDxfer)
    'SPITransfer (0xFF, dummy)


    #IFDEF Debug_SD_OCR
      HSerPrint "("
      HSerPrint SDarg(1):HSerPrint ", "
      HSerPrint SDarg(2):HSerPrint ", "
      HSerPrint SDarg(3):HSerPrint ", "
      HSerPrint SDarg(4)
      HSerPrint ")"
      HSerPrintCRLF
    #ENDIF

  End If

  Set SD_CS On

  If SD_ACMD41 = True Then
    #IfDef Debug_SD_OCR
      HSerPrint "Card is..."
    #ENDIF

    If SDarg(1) = 128 OR SDarg(1) = 129 Then
      #IfDef Debug_SD_OCR
        HSerPrint "SDSC"
      #ENDIF
    End If

        'If SDarg(1) AND 0xFE = 192 Then
    If SDarg(1) = 192 OR SDarg(1) = 193 Then
      #IfDef Debug_SD_OCR
        HSerPrint "SDHC or SDXC"
      #ENDIF
      SD_SDHC = True
    End If

    If SDarg(1) AND 0x01 = 1 Then
      #IfDef Debug_SD_OCR
        HSerPrint " UHS-1"
      #ENDIF
    End If

    If SDarg(1) = 224 Then
      #IfDef Debug_SD_OCR
        HSerPrint "SDHC or SCXC UHS-II"
      #ENDIF
    End If

    If SDphysicalV2 = True Then
      #IfDef Debug_SD_OCR
        HSerPrint " V2"
      #ENDIF
    Else
      #IfDef Debug_SD_OCR
        HSerPrint " V1"
      #ENDIF
    End If
    HSerPrintCRLF
  End If

end sub


sub SD_Read_CID

'********************
'CMD10 SEND_CID
'send card identification information
'********************
Dim DateYr as Word

Call SD_SendCMD (0x4A, 0X00, 0X00, 0X00, 0XFF, 0XFF)
'Set SD_CS Off
'
'  fOR TRY = 1 TO 5
'    'delay before sending command
'    SPITransfer (0xFF, dummy)
'    'CMD10 Send_CID
'    SPITransfer (0x4A, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0xFF, dummy)
'    SPITransfer (0xFF, dummy)
'
'    'wait till R1 response idle bit cleared
'    repeat 5
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 0 Then exit Repeat
'    end repeat
'
'    If SDresponse = 0 Then exit For
'
'  next
'
'  'HSerPrint "(":HSerPrint SDresponse:HSerPrint ")"
'
'  fOR TRY = 1 TO 255
'    'wait for Data Block Transfer - first byte start block token
'    repeat 5
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 0xFE Then exit Repeat
'      wait 1 ms
'    end repeat
'    If SDresponse = 0xFE Then exit For
'  Next

  'HSerPrint "(":HSerPrint SDresponse:HSerPrint ")"

  'SPITransfer (0xFF, SDresponse)    'Receive 16bit CRC
  'SPITransfer (0xFF, SDresponse)

  If SDresponse <> 0xFE Then
    HSerPrint "(":HSerPrint SDresponse:HSerPrint ")"
    HSerPrint "Read CID failed!!!"
    HSerPrintCRLF
  End If


  For arg = 1 to 16
    SPITransfer (0xFF, SDresponse)
    SDarg(arg) = SDresponse
  Next

  SPITransfer (0xFF, dummy)


  HSerPrintCRLF
  HSerPrint "***Card Indentification Register (CID)***":HSerPrintCRLF
  HSerPrint "("
  HSerPrint SDarg(1):HSerPrint ", "
  HSerPrint SDarg(2):HSerPrint ", "
  HSerPrint SDarg(3):HSerPrint ", "
  HSerPrint SDarg(4):HSerPrint ", "
  HSerPrint SDarg(5):HSerPrint ", "
  HSerPrint SDarg(6):HSerPrint ", "
  HSerPrint SDarg(7):HSerPrint ", "
  HSerPrint SDarg(8)
  HSerPrint ")"
  HSerPrintCRLF

  HSerPrint "("
  HSerPrint SDarg(9):HSerPrint ", "
  HSerPrint SDarg(10):HSerPrint ", "
  HSerPrint SDarg(11):HSerPrint ", "
  HSerPrint SDarg(12):HSerPrint ", "
  HSerPrint SDarg(13):HSerPrint ", "
  HSerPrint SDarg(14):HSerPrint ", "
  HSerPrint SDarg(15):HSerPrint ", "
  HSerPrint SDarg(16)
  HSerPrint ")"
  HSerPrintCRLF

  HSerPrint "Name: ":Call SD_ManfID SDarg(1):HSerPrintCRLF
  HSerPrint "MID = ":HSerPrint "0x":HSerPrint Hex SDarg(1):HSerPrintCRLF
  HSerPrint "OID = ":HSerPrint CHR(SDarg(2)):HSerPrint CHR(SDarg(3)):HSerPrintCRLF
  HSerPrint "PNM = ":HSerPrint CHR(SDarg(4)):HSerPrint CHR(SDarg(5))
  HSerPrint CHR(SDarg(6)):HSerPrint CHR(SDarg(7)):HSerPrint CHR(SDarg(8)):HSerPrintCRLF
  HSerPrint "PRV = 0x":HSerPrint Hex (SDarg(9)):HSerPrintCRLF
  HSerPrint "PSN = 0x":HSerPrint Hex (SDarg(10)): HSerPrint Hex (SDarg(11))
  HSerPrint Hex (SDarg(12)):HSerPrint Hex (SDarg(13)):HSerPrintCRLF
  DateYr = (SDarg(14) AND 0x0F * 16):DateYr = DateYr + SDarg(15)/16:DateYr = 2000 + DateYr
  Mth = SDarg(15) AND 0x0F
  HSerPrint "MDT = ":Call SD_DateMo(Mth):HSerPrint " ":HSerPrint DateYr:HSerPrintCRLF
  HSerPrint "CRC7 = 0x":HSerPrint (SDarg(16) - 1):HSerPrintCRLF

  Set SD_CS On

end sub


sub SD_Read_CSD

''********************
''CMD9 SEND_CSD_Structure, send card specific data structure
''CSD structure different for V1 and V2 type cards
''********************
'Set SD_CS Off
'fOR TRY = 1 TO 250
'    'delay before sending command
'    SPITransfer (0xFF, dummy)
'    'CMD9 Send_CSD
'    SPITransfer (0x49, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0x00, dummy)
'    SPITransfer (0xFF, dummy)
'    SPITransfer (0xFF, dummy)
'
'  'wait till R1 response idle bit cleared
'  repeat 5
'    SPITransfer (0xFF, SDresponse)
'    If SDresponse = 0 Then exit Repeat
'  end repeat
'    If SDresponse = 0 Then exit For
'
'Next
'
'fOR TRY = 1 TO 255
'  'wait for Data Block Transfer - first byte start block token
'  repeat 25
'    SPITransfer (0xFF, SDresponse)
'    If SDresponse = 0xFE Then exit Repeat
'  end repeat
'  If SDresponse = 0xFE Then exit For
'Next
'    For arg = 1 to 16
'      SPITransfer (0xFF, SDresponse)
'      SDarg(arg) = SDresponse
'      'nop
'      'nop
'    Next
'
'    SPITransfer (0xFF, dummy)
'
'      HSerPrintCRLF
'      HSerPrint "***Card Specific Data Structure (CSD)***":HSerPrintCRLF
'      HSerPrint "("
'      HSerPrint SDarg(1):HSerPrint ", "
'      HSerPrint SDarg(2):HSerPrint ", "
'      HSerPrint SDarg(3):HSerPrint ", "
'      HSerPrint SDarg(4):HSerPrint ", "
'      HSerPrint SDarg(5):HSerPrint ", "
'      HSerPrint SDarg(6):HSerPrint ", "
'      HSerPrint SDarg(7):HSerPrint ", "
'      HSerPrint SDarg(8)
'      HSerPrint ")"
'    HSerSend 13
'    HSerSend 10
'      HSerPrint "("
'      HSerPrint SDarg(9):HSerPrint ", "
'      HSerPrint SDarg(10):HSerPrint ", "
'      HSerPrint SDarg(11):HSerPrint ", "
'      HSerPrint SDarg(12):HSerPrint ", "
'      HSerPrint SDarg(13):HSerPrint ", "
'      HSerPrint SDarg(14):HSerPrint ", "
'      HSerPrint SDarg(15):HSerPrint ", "
'      HSerPrint SDarg(16)
'      HSerPrint ")"
'    HSerSend 13
'    HSerSend 10
'Dim CCC as Word
'Dim Taac as Word
'Dim TimeUnit(8)
'Dim TimeVal(15)
'Dim TranSpUnit(4)
'TimeUnit = 1,10,100,1,10,100,1,10
'TimeVal = 10,12,13,15,20,25,30,35,40,45,50,55,60,70,80
'TranSpUnit = 100,1,10,100
'
'  CSD = SDarg(1)
'  HSerPrint "CSD = ":HSerPrint CSD
'  If CSD = 0 Then
'    HSerPrint " Standard"
'  Else
'    HSerPrint " High or Extended"
'  End If
'  HSerPrint " Capacity":HSerPrintCRLF
'
'  HSerPrint "TAAC = ":HSerPrint HEX SDarg(2)
'    TU = (SDarg(2) AND 0x07) + 1
'    TV = SDarg(2) AND 0x78: TV = TV / 8
'    Taac = TimeVal(TV) * TimeUnit(TU)
'  HSerPrint "... ":HSerPrint Taac / 10:HSerPrint ".":HSerPrint Taac%10
'    If TU < 4 Then HSerPrint " ns"
'    If TU > 3 AND TU < 7 Then HSerPrint " us"
'    If TU > 6 Then HSerPrint " ms"
'    HSerPrint " (data access time)"
'    HSerPrintCRLF
'
'  HSerPrint "NSAC = 0x":HSerPrint Hex SDarg(3):HSerPrintCRLF
'
'  HSerPrint "TRAN_SPEED = 0x":HSerPrint Hex SDarg(4)
''    TU = (SDarg(4) AND 0x07) + 1
''    TV = SDarg(4) AND 0x78: TV = TV / 8
''    Tran_Speed = TimeVal(TV) * TranSpUnit(TU)
''  HSerPrint "...":HSerPrint Tran_Speed / 10:HSerPrint ".":HSerPrint Tran_Speed%10
'    If SDarg(4) = 0x32 Then HSerPrint "... 25Mbs"
'    If SDarg(4) = 0x5A Then HSerPrint " 50Mbs"    '???
'    If SDarg(4) = 0x0B Then HSerPrint " 100Mbs"
'    If SDarg(4) = 0x2B Then HSerPrint " 200Mbs"
'    HSerPrint " (max transfer rate)"
'    HSerPrintCRLF
'
'  HSerPrint "CCC = b": HSerPrint ByteToBin(SDarg(5))
'  HSerPrint "0101 (card command classes)":HSerPrintCRLF
'  'HSerPrint ByteToBin(CCC_L):HSerPrintCRLF
'  'BlockLen = SDarg(6) AND 0x0F
'  If SDphysicalV2 = True Then
'    'SDarg(6) AND 0x0F is fixed at "9"
'    HSerPrint "Read_BL_Len = 512 Bytes":HSerPrintCRLF  ':HSerPrint Hex (BlockLen):HSerPrintCRLF
'    'SDarg(7) B7:B6:B5 fixed at zero
'    HSerPrint "READ_BL_PARTIAL = FALSE":HSerPrintCRLF ':HSerPrint Hex (PartialBl):HSerPrintCRLF
'    HSerPrint "WRITE_BLK_MISALIGN = FALSE":HSerPrintCRLF
'    HSerPrint "READ_BLK_MISALIGN = FALSE":HSerPrintCRLF
'  End If
'
'  ArrayBits = SDarg(7) AND 0x10
'  HSerPrint "DSR_IMP = "
'  If ArrayBits = 1 Then
'    HSerPrint "TRUE"
'  Else
'    HSerPrint "FALSE"
'  End If
'  HSerPrint " (configurable driver stage)":HSerPrintCRLF
'
'
'  If SDphysicalV2 = True Then
'    HSerPrint "C_SIZE = 0x"
'    'ArrayNibL = SDarg(87) AND 0x0F
'    ArrayBits = SDarg(8) AND 0x3F
'    HSerPrint ArrayBits:HSerPrint Hex SDarg(9):HSerPrint Hex SDarg(10)
'    HSerPrint " (card size)":HSerPrintCRLF
'
'    'ArrayBits = SDarg(11) AND 0x3F
'    HSerPrint "ERASE_BLK_EN = 512 Bytes":HSerPrintCRLF
'    HSerPrint "SECTOR_SIZE = 64kBytes":HSerPrintCRLF
'    HSerPrint "WP_GRP_SIZE = FALSE":HSerPrintCRLF
'    HSerPrint "WP_GRP_ENABLE = FALSE":HSerPrintCRLF
'
'    HSerPrint "R2W_FACTOR = 4 (write speed factor)":HSerPrintCRLF
'    HSerPrint "WRITE_BL_LEN = 512 Bytes":HSerPrintCRLF
'    HSerPrint "WRITE_BL_PARTIAL = FALSE":HSerPrintCRLF
'    HSerPrint "FILE_FORMAT_GRP = FALSE":HSerPrintCRLF
'    HSerPrint "COPY = ":HSerPrintCRLF
'    HSerPrint "PERM_WRITE_PROTECT = ":HSerPrintCRLF
'    HSerPrint "TMP_WRITE_PROTECT = ":HSerPrintCRLF
'    HSerPrint "FILE_FORMAT = FALSE":HSerPrintCRLF
'    CRC7 = (SDarg(16) - 1) / 2
'    HSerPrint "CRC = 0x":HSerPrint Hex(CRC7):HSerPrintCRLF
'    HSerPrintCRLF
'  End If
''  'SDarg(11)  '????
''
''    HSerPrint CsizeHnib:HSerPrint SDarg(8):HSerPrintCRLF
''
''  HSerPrint Hex (SDarg(12)):HSerPrint Hex (SDarg(13)):HSerPrintCRLF
''  DateYr = (SDarg(14) AND 0x0F * 16):DateYr = DateYr + SDarg(15)/16:DateYr = 2000 + DateYr
''  Mth = SDarg(15) AND 0x0F
''  HSerPrint "MDT = ":Call SD_DateMo(Mth):HSerPrint " ":HSerPrint DateYr:HSerPrintCRLF
''  HSerPrint "CRC7 = 0x":HSerPrint (SDarg(16) - 1)
'Set SD_CS On

end sub


sub SD_Read_SCR

'********************
'CMD ACMD51 SEND_SCR, Reads the SD Configuration Register
'********************
'ACMD51 = CMD55 plus CMD51

  Call SD_SendCMDR0 (0x77, 0x00, 0x00, 0x00, 0x00, 0xFF)

  Call SD_SendCMD (0x73, 0x40, 0x00, 0x00, 0x00, 0xFF)

  If SDresponse <> 0xFE Then
    HSerPrintCRLF
    HSerPrint "(":HSerPrint SDresponse:HSerPrint ")"
    HSerPrint "Read SCR failed!!!"
  End If

  For arg = 1 to 8
    SPITransfer (0xFF, SDresponse)
    SDarg(arg) = SDresponse
  Next

  SPITransfer (0xFF, dummy)

  HSerPrintCRLF
  HSerPrint "***Card Configurattion Register (SCR)***":HSerPrintCRLF
  HSerPrint "("
  HSerPrint SDarg(1):HSerPrint ", "
  HSerPrint SDarg(2):HSerPrint ", "
  HSerPrint SDarg(3):HSerPrint ", "
  HSerPrint SDarg(4):HSerPrint ", "
  HSerPrint SDarg(5):HSerPrint ", "
  HSerPrint SDarg(6):HSerPrint ", "
  HSerPrint SDarg(7):HSerPrint ", "
  HSerPrint SDarg(8)
  HSerPrint ")"
  HSerPrintCRLF
'
'    SCR_STRUCTURE = SDarg(1) AND 0xF0
'    HSerPrint SCR_STRUCTURE:HSerPrint " "
    SD_SPEC = SDarg(1) AND 0x0F
'    HSerPrint SD_SPEC
'    HSerPrintCRLF
'
'    DATA_STAT_AFTER_ERASE = SDarg(2) AND 0x80
'    HSerPrint DATA_STAT_AFTER_ERASE:HSerPrint " "
'    SD_SECURITY = (SDarg(2) AND 0x70) / 16
'    HSerPrint SD_SECURITY:HSerPrint " "
'    SD_BUS_WIDTHS = SDarg(2) AND 0x0F
'    HSerPrint SD_BUS_WIDTHS
'    HSerPrintCRLF
'
    SD_SPEC3 = (SDarg(3) AND 0x80) / 128
'    HSerPrint SD_SPEC3:HSerPrint " "
'    EX_SECURITY = (SDarg(3) AND 0x78) / 8
'    HSerPrint EX_SECURITY:HSerPrint " "
'    EX_SECURITY = (SDarg(3) AND 0x04) / 4
'    HSerPrint EX_SECURITY:HSerPrint " "
    SD_SPEC4 = SDarg(3) AND 0x04
    SD_SPECX = SDarg(3) AND 0x03
    SD_SPECX = SD_SPECX + (SDarg(4) AND 0xC0) / 64
'    HSerPrint SD_SPECX:HSerPrint " "
'    HSerPrintCRLF
'
'   CMD_SUPPORT = SDarg(4) AND 0x0F
'    HSerPrint CMD_SUPPORT:HSerPrint " "
'    HSerPrintCRLF
  'Call Version
  PhySpec = SD_Spec3 + SD_Spec4 + SD_SpecX

  HSerPrint "Physical Layer Spec Version "
  If SD_SPEC = 0 Then HSerPrint "1.0 AND 1.01"
  If SD_SPEC = 1 Then HSerPrint "1.10"
  If SD_SPEC = 2 AND PhySpec = 0 Then HSerPrint "2.00"
  If SD_SPEC = 2 AND SD_SPEC3 = 1 Then HSerPrint "3.0X"
  If SD_SPEC = 2 AND SD_SPEC4 = 1 Then HSerPrint "4.XX"
  If SD_SPEC = 2 AND SD_SpecX = 1 Then HSerPrint "5.XX"
  If SD_SPEC = 2 AND SD_SpecX = 2 Then HSerPrint "6.XX"
  HSerPrintCRLF
  HSerPrintCRLF

Set SD_CS On

end sub


'*********************Software SPI**********************
sub SPI_Xfer(xfer)

  For clocks = 1 to 8

    If xfer.7 = 1 Then
      SD_MOSI = 1
      wait 3 us
    Else
      SD_MOSI = 0
      wait 3 us
    End if
    Rotate xfer Left Simple
    SD_SCK = 1

    If SD_MISO = 1 Then
      xfer.0 = 1
      wait 3 us
    Else
      xfer.0 = 0
      wait 3 us
    End If
   SD_SCK = 0

  Next

end sub


sub SD_ReadBMP(In StartAddr as Long)
'********************
'CMD17 Read_Single_Block
'Read a block of BMP image data
'********************
Dim arg as Word
Dim argx as Word
Dim Try as Word
BMPAddr = True
FileDataAddr = StartAddr

  #IfDef Debug_SD_ReadBMP
    HSerPrint "Receiving Block Data....":HSerPrintCRLF
  #EndIf

  Call SD_SendCMD (0x51, StartAddr_E, StartAddr_U, StartAddr_H, StartAddr, 0xFF)

  #IfDef Debug_SD_ReadBMP
    HSerPrint "(":HSerPrint SDresponse:HSerPrint ") "
    If SDresponse = 0xFE Then
        'HSerPrint "(":HSerPrint SDresponse:HSerPrint ") "
        HSerPrint "read data success"
      Else
        'HSerPrint "(":HSerPrint SDresponse:HSerPrint ") "
        HSerPrint "read data failure"
      End If
    HSerPrintCRLF
  #endif

  For arg = 1 to 512
    SPITransfer (0xFF, SDresponse)
    Data(arg) = SDresponse
  Next

  SPITransfer (0xFF, SDresponse)    'Receive 16bit CRC
  SPITransfer (0xFF, SDresponse)    'and back to idle?

  Set SD_CS On

  'allow data even if not aligned?
  If Data(1) And Data(2) = 47 Then   'forward slash's of file header

    rowcount = 0
    For arg = 1 to 512

      rowcount += 1
      #IFDEF Debug_SD_ReadBMP
        HSerPrint CHR Data(arg):HSerPrint " "
        If rowcount = 16 Then
          rowcount = 0
          HSerPrintCRLF
        End If
      #ENDIF

      If Data(arg) = 123 Then   'left opening brace
        exit For
      End If

    Next

    'find operning prefix of hex value "0"
    Do Until Data(arg) = 48
      arg += 1
    Loop

  End If

  'document the beginning of "first" values of data block
  SD_EOF = False
  BMPAddr = False
  DiscardHeader = False
  FileStartAddr = True
  argx = 0
  count = 0
  commacount = 0
  rowcount = 0
  sectorcount = 0
  filesize = 0
  datablks = 0

  #IFDEF Debug_SD_ReadBMP
    HSerPrintCRLF
    Call SD_WordBlockHeader(FileDataAddr)
    FileStartAddr = False
    HSerPrint HEX(rowcount):HSerPrint "0 "
  #ENDIF

      'right closing brace has been reached
      Do Until SD_EOF = True

        'make 256 word table
        Do Until rowcount = 16
          'HSerPrint Data(arg)
          'set conditions for hex number profile
          If Data(arg) = 48 Then  'zero
            If arg = 512 Then Call SD_GetMoreData (StartAddr)
            arg += 1

            If Data(arg) = 120 Then    'lower case x
              'HSerPrint CHR (Data(arg))
              If arg = 512 Then Call SD_GetMoreData (StartAddr)
              arg += 1

              'first char of upper Byte of Word size hex number
              uppernib = Data(arg)

              #IFDEF Debug_SD_ReadBMP
                HSerPrint CHR (uppernib)
              #ENDIF

              If arg = 512 Then Call SD_GetMoreData (StartAddr)
              arg += 1

              'second char of upper Byte of Word size hex number
              lowernib = Data(arg)

              #IFDEF Debug_SD_ReadBMP
                HSerPrint CHR (lowernib)
              #ENDIF

              If arg = 512 Then Call SD_GetMoreData (StartAddr)
              arg += 1

              'argx keeps track of parsed bytes
              argx += 1
              SDarg(argx) = ByteHex2Dec

              'first char of lower Byte of Word size hex number
              uppernib = Data(arg)

              #IFDEF Debug_SD_ReadBMP
                HSerPrint CHR (uppernib)
              #ENDIF

              If arg = 512 Then Call SD_GetMoreData (StartAddr)
              'second char of lower Byte of Word size hex number
              arg += 1
              lowernib = Data(arg)

              #IFDEF Debug_SD_ReadBMP
                HSerPrint CHR (lowernib)
              #ENDIF

              If arg = 512 Then Call SD_GetMoreData (StartAddr)

              'argx keeps track of parsed bytes
              argx += 1
              SDarg(argx) = ByteHex2Dec
            End If

            'discard comma after 0xXXXX value
            arg += 1
            commacount += 1

            #IFDEF Debug_SD_ReadBMP
              HSerPrint " "
            #ENDIF

            If arg = 512 Then Call SD_GetMoreData (StartAddr)
          End If

          '************************************
          '* NOTE: No preamble or header list *
          '* with Image Converter 565 files   *
          '************************************
          If argx = 512 Then
            argx = 0
            sectorcount += 1
            Call XferSDData
          End If

          'discard space, make ready for next conditional
          Repeat 2
          'reached End Of File, right closing brace!!!
          If Data(arg) = 125 Then exit sub
            If arg = 512 Then Call SD_GetMoreData (StartAddr)
            arg += 1
          End Repeat

          If Data(arg) = 10 Then   'line feed after X entries
            If arg = 512 Then Call SD_GetMoreData  (StartAddr)
            arg += 1
          End If

          Do While Data(arg) = 32   'extra space after X entries
            If arg = 512 Then Call SD_GetMoreData (StartAddr)
            arg += 1
          Loop

          If commacount = 16 Then
            commacount = 0
            rowcount += 1

            Do While Data(arg) <> 41  'right closing parenthesis   'carriage return 'line feed
              If arg = 512 Then Call SD_GetMoreData (StartAddr)
              arg += 1
              'HSerPrint Data(arg):HSerPrint " "
            Loop

            Do Until Data(arg) = 48
              If arg = 512 Then Call SD_GetMoreData (StartAddr)
              arg += 1
              'HSerPrint Data(arg):HSerPrint " "
            Loop

            #IfDef Debug_SD_ReadBMP
              HSerPrintCRLF
            #EndIf

            If rowcount < 16 Then

              #IFDEF Debug_SD_ReadBMP
                HSerPrint HEX(rowcount):HSerPrint "0 "
              #ENDIF

            End If

            End If

        Loop
        rowcount = 0

        #IFDEF Debug_SD_ReadBMP
            Call SD_WordBlockHeader(FileDataAddr)
            HSerPrint HEX(rowcount):HSerPrint "0 "
        #ENDIF

        '#IFDEF Debug_SD_FileAttributes
        '      If SectorCount = 1 Then
        '        HSerPrint HEX (SectorCount_H)
        '        HSerPrint HEX (SectorCount)
        '      Else
        '        HSerSend 8
        '        wait 8 ms
        '        HSerSend 8
        '        wait 8 ms
        '        HSerSend 8
        '        wait 8 ms
        '        HSerSend 8
        '        wait 8 ms
        '        HSerPrint HEX (SectorCount_H)
        '        HSerPrint HEX (SectorCount)
        '      End If
        '#ENDIF

      Loop

      #IFDEF Debug_SD_FileAttributes
        If SD_EOF = True Then
          HSerPrintCRLF
          HSerPrint "exit BMP":HSerPrintCRLF
          HSerPrintCRLF
          HSERPrintCRLF
        End If
      #ENDIF

end sub


sub SD_SendCMDR0 (IN SDCMD, IN SDargmntE, IN SDargmntU, IN SDargmntH, IN SDargmnt, IN SDCRC7)
  Set SD_CS Off

  For TryAgain = 1 to 50
    'delay before sending command
    SPITransfer (0xFF, dummy)
    'CMD8 Send_IF_COND
    'SD V1.x spec does not recognize CMD8
    'SD V2.0 and up supports CMD8
    SPITransfer (SDCMD, dummy)
    SPITransfer (SDargmntE, dummy)
    SPITransfer (SDargmntU, dummy)
    SPITransfer (SDargmntH, dummy)
    SPITransfer (SDargmnt, dummy)
    SPITransfer (SDCRC7, dummy)

    repeat 5
      SPITransfer (0xFF, SDresponse)
      If SDresponse = 0 Then exit repeat
    end repeat

    If SDresponse = 0 Then exit For

  Next

end sub


sub SD_SendCMDR1 (IN SDCMD, IN SDargmntE, IN SDargmntU, IN SDargmntH, IN SDargmnt, IN SDCRC7)

  Set SD_CS Off


  For TryAgain = 1 to 50
    'delay before sending command
    SDxfer = 0xFF
    SPI_Xfer (SDxfer)
    'CMD8 Send_IF_COND
    'SD V1.x spec does not recognize CMD8
    'SD V2.0 and up supports CMD8
    SPI_Xfer (SDCMD)
    SPI_Xfer (SDargmntE)
    SPI_Xfer (SDargmntU)
    SPI_Xfer (SDargmntH)
    SPI_Xfer (SDargmnt)
    SPI_Xfer (SDCRC7)

    Repeat 5
      SDxfer = 0xFF
      SPI_Xfer (SDxfer)

      If SD_ACMD41 = True Then 'SDCMD = 0x7A Then
      'If SDCMD = 0x69 OR SD_ACMD41 = True Then 'SDCMD = 0x7A Then
        'If ACMD41 = True Then
            'want to use Software SPI till after ACMD41 and 2nd CMD58 Initialization
           If SDxfer = 0 Then exit Repeat
        Else
           If SDxfer = 1 Then exit Repeat
        'End If
      End If
    End Repeat

'      If SDCMD = 0x69 Then  'OR SDCMD = 0x7A Then
'        'want to use Software SPI till after ACMD41 Initialization
'        If SDxfer = 0 Then exit For
'      Else
       If SDxfer = 1 OR SDxfer = 5 Then exit For
'      End If
  'If SDxfer = 1 OR SDxfer = 5 Then exit For
          If SD_ACMD41 = True AND SDxfer = 0 Then exit For
      'Else
         'If SDCMD = 0x69 AND SDxfer = 0 Then exit For
'         IF SDCMD <> 0x69 AND SDxfer = 5 Then exit FOR
  Next


'  For TryAgain = 1 to 50
'    'delay before sending command
'    SPITransfer (0xFF, dummy)
'    'CMD8 Send_IF_COND
'    'SD V1.x spec does not recognize CMD8
'    'SD V2.0 and up supports CMD8
'    SPITransfer (SDCMD, dummy)
'    SPITransfer (SDargmntE, dummy)
'    SPITransfer (SDargmntU, dummy)
'    SPITransfer (SDargmntH, dummy)
'    SPITransfer (SDargmnt, dummy)
'    SPITransfer (SDCRC7, dummy)
'
'    repeat 5
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 1 OR SDresponse = 5 Then exit Repeat
'    end repeat
'
'    If SDresponse = 1 OR SDresponse = 5 Then exit For
'
'  Next

end sub



sub SD_SendCMD (IN SDCMD, IN SDargmntE, IN SDargmntU, IN SDargmntH, IN SDargmnt, IN SDCRC7)

  Set SD_CS Off

  fOR TRY = 1 TO 5
    'delay before sending command
    SPITransfer (0xFF, dummy)
    'CMD10 Send_CID
    SPITransfer (SDCMD, dummy)
    SPITransfer (SDargmntE, dummy)
    SPITransfer (SDargmntU, dummy)
    SPITransfer (SDargmntH, dummy)
    SPITransfer (SDargmnt, dummy)
    SPITransfer (SDCRC7, dummy)

    'wait till R1 response idle bit cleared
    repeat 5
      SPITransfer (0xFF, SDresponse)
      If SDresponse = 0 Then exit Repeat
    end repeat

    If SDresponse = 0 Then exit For

  next

  'HSerPrint "(":HSerPrint SDresponse:HSerPrint ")"

  fOR TRY = 1 TO 1024 '512  '255
    'wait for Data Block Transfer - first byte start block token
    repeat 5
      SPITransfer (0xFF, SDresponse)
      If SDresponse = 0xFE Then exit Repeat
      wait 1 ms
    end repeat
    If SDresponse = 0xFE Then exit For
  Next

end sub


sub SD_PrintFiles

  LastEEAddr = 7
  If SD_NumFiles > 0 Then

    HSerPrintCRLF
    HSerPrint "FileNames and Start Location Offsets:"
    HSerPrintCRLF
    For nextfile = 1 to SD_NumFiles

      HSerPrintCRLF
      For nextchr = 1 to 8
        LastEEAddr += 1
        EPRead LastEEAddr, SD_name
        HSerPrint CHR (SD_name)
      Next

      'If FAT32 = True OR SD_SDHC = True Then
      If FAT32 = True Then

        HSerPrint "  Sector"
      Else
        HSerPrint "  Byte"
      End If

      HSerPrint " 0x"
      For nextchr = 1 to 4
        LastEEAddr += 1
        EPRead LastEEAddr, SD_Offset
        HSerPrint HEX (SD_Offset)

      Next

    Next

  End If

  HSerPrintCRLF
  HSerPrintCRLF
  HSerPrintCRLF
end sub


function ByteHex2Dec
  uppernib = uppernib - 48
  If uppernib =< 9 Then
    Hex2DecH = uppernib
  Else
    'assume uppercase A thru F
    Hex2DecH = uppernib - 7
  End If

  lowernib = lowernib - 48
  If lowernib =< 9 Then
    Hex2DecL = lowernib
  Else
    'assume uppercase A thru F
    Hex2DecL = lowernib - 7
  End If

  ByteHex2Dec = (Hex2DecH * 16) + Hex2DecL
end function


function Hex2Dec
  nib = nib - 48
  If nib =< 9 Then
    Hex2Dec = nib
  Else
    'assume uppercase A thru F
    Hex2Dec = (nib - 7)
  End If

end function

sub SD_GetMoreData (StartAddr)

  If FAT32 = True OR SD_SDHC = True Then
    StartAddr += 1
  Else
    'FAT16
    StartAddr = StartAddr + [Long]512
  End If

  Call SD_SendCMD (0x51, StartAddr_E, StartAddr_U, StartAddr_H, StartAddr, 0xFF)

  'MoreData:
  For arg = 1 to 512
    SPITransfer (0xFF, SDresponse)
    Data(arg) = SDresponse
  Next

  SPITransfer (0xFF, SDresponse)    'Receive 16bit CRC
  SPITransfer (0xFF, SDresponse)

  'test for end of file at closing right brace"}"
  For arg = 1 to 512
    If Data(arg) = 125 Then
      SD_EOF = True
      Exit For
    End If
  Next
  arg = 0

  Set SD_CS On

end sub


sub SD_WriteBlock
''********************
''CMD24 Write_Block, write a block of data
''
''********************
''Dim SDarg(128)
'Dim arg as Word
'
'  HSerPrint "Sending Block Data....":HSerPrintCRLF
'Set SD_CS Off
'    'delay before sending command
'    SPITransfer (0xFF, dummy)
'    'CMD24 Write_Block
'    SPITransfer (0x58, dummy)
'    SPITransfer (0x00, dummy)   'Start address argument
'    SPITransfer (0x00, dummy)   'Start address argument
'    SPITransfer (0x02, dummy)   'Start address argument
'    SPITransfer (0x00, dummy)   'Start address argument
'    SPITransfer (0xFF, dummy)
'
'  For Try = 1 to 255
'    'wait for command response
'    repeat 5
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 0x00 Then exit Repeat
'    end repeat
'    If SDresponse = 0x00 Then exit For
'  Next
'
'  'delay => than 1Byte before sending packet
'  Repeat 5
'    SPITransfer (0xFF, SDresponse)
'  End Repeat
'  SPITransfer (0xFE, SDresponse)    'Data Token for CMD17/18/24
'    For arg = 1 to 128
'      SDarg(arg) = arg  '+10
'      SPITransfer (SDarg(arg), SDresponse)
'    Next
'
'    For arg = 129 to 512
'      SPITransfer (0x00, SDresponse)
'    Next
'  SPITransfer (0xFF, SDresponse)    'Send 16bit CRC
'  SPITransfer (0xFF, SDresponse)
'
'    'SD card response token after block write
'    SPITransfer (0xFF, SDresponse)
'    SDresponse = SDresponse AND 0x0F
'    HSerPrint "(":HSerPrint SDresponse:HSerPrint ") "
'    If SDresponse = 5 Then    '???
'      HSerPrint "write data success":HSerPrintCRLF
'    Else
'      HSerPrint "write data failure":HSerPrintCRLF
'    End If
'
'
'  'this is nonsense conditional?
'  'should calc wait time based on read speed of data block(s)
'  fOR TRY = 1 TO 1024
'    'wait for card not busy
'    repeat 5
'      SPITransfer (0xFF, SDresponse)
'      If SDresponse = 0xFF Then exit Repeat
'    end repeat
'    If SDresponse = 0xFF Then Exit For
'  Next
'  'HSerSend SDresponse:HSerPrintCRLF
'Dim StartAddr as Long
''address *** M U S T *** land on 512 Byte (i.e. Block) boundary so the 3rd LSB
''must be an even value like 000, 200, 400, 600, 800, A00, C00, E00
'#define BootBlkAddr 0x00
''#define RootDIRAddr 0x3B000
'#define FileDatAddr 0x4F000
'
''StartAddr = 0x0003B000
''Call SD_GetRootDir
'
''StartAddr = 0x00
''Call SD_ReadBlock (StartAddr)
'Call SD_ReadBlock (BootBlkAddr)
'
'HSerPrint "Root Directory Address 0x"
'HSerPrint Hex (StartAddr_E):HSerPrint Hex (StartAddr_U)
'HSerPrint Hex (StartAddr_H):HSerPrint Hex (StartAddr)
'HSerPrintCRLF
'Call SD_ReadBlock (RootDIRAddr)
'Call SD_ReadBlock (FileDatAddr)
  'SPITransfer (0xFF, SDresponse)


'    For arg = 1 to 128
'      HSerPrint Data(arg):HSerPrint ","
'      If arg = 32 Then HSerPrintCRLF
'      If arg = 64 Then HSerPrintCRLF
'      If arg = 96 Then HSerPrintCRLF
'      If arg = 128 Then HSerPrintCRLF
'      If arg = 160 Then HSerPrintCRLF
'      If arg = 192 Then HSerPrintCRLF
'      If arg = 224 Then HSerPrintCRLF
'      If arg = 256 Then HSerPrintCRLF
'      'If arg = 96 Then HSerPrintCRLF
'    Next



'    arg = 0
'    For ByteX16 = 0 to 7
'      HSerPrint Hex (count):HSerPrint "0"
'      count += 1
'
'      For count1 = 0 to 15
'        arg += 1
'        HSerPrint " ":HSerPrint HEX (Data(arg))
'        If arg = 16 Then HSerPrintCRLF
'        If arg = 32 Then HSerPrintCRLF
'        If arg = 48 Then HSerPrintCRLF
'        If arg = 64 Then HSerPrintCRLF
'        If arg = 80 Then HSerPrintCRLF
'        If arg = 96 Then HSerPrintCRLF
'        If arg = 112 Then HSerPrintCRLF
'        If arg = 128 Then HSerPrintCRLF
'      Next
'
'    Next
'Set SD_CS On

end sub


sub SD_ReadBlock (In StartAddr as Long)
'********************
'CMD17 Read_Single_Block, Read a block of data
'
'********************
'Dim BSEBlkAddr as Long
Dim arg as Word
Dim Try as Word

  #IFDEF Debug_SD_ReadBlock
    HSerPrintCRLF
    HSerPrint "Receiving Block Data....":HSerPrintCRLF
    'delay before sending command
  #EndIf

  Call SD_SendCMD (0x51, StartAddr_E, StartAddr_U, StartAddr_H, StartAddr, 0xFF)

  #IFDEF Debug_SD_ReadBlock
    HSerPrint "(":HSerPrint SDresponse:HSerPrint ") "
    If SDresponse = 0xFE Then
      'HSerPrint "(":HSerPrint SDresponse:HSerPrint ") "
      HSerPrint "read data success"
    Else
      'HSerPrint "(":HSerPrint SDresponse:HSerPrint ") "
      HSerPrint "read data failure"
    End If
    HSerPrintCRLF
    'HSerPrint SDresponse:HSerPrintCRLF
    'Call SD_BlockHeader(StartAddr)
  #ENDIF

  count = 0

  'Recycle smaller data arrays to make 512 Byte Block/Sector
  For recycle = 1 to 4

    For arg = 1 to 128
      SPITransfer (0xFF, SDresponse)
      Data(arg) = SDresponse
    Next

    'pick off first byte of array to detemine
    'whether this is a MasterBootRecord(MBR) or it
    'is a BootSectorEntry(BSE).  If first Byte is
    '0xEB then it is BootSectorEntry, else it is a MBR.
    'If MBR true, then the BSE offset needs to be calculated
    'and the new BSE block needs to be read
    If StartAddr = 0 OR StartAddr = BSEBlkAddr Then
      If recycle = 1 Then
        If Data(1) <> 0xEB Then
          SD_MBR = True
        End If

        If Data(1) = 0xEB Then

          SD_BSE = True
          'If no MBR, (Win XP format?) Then get FAT type here
          If Data(0x37) = 70 Then      ' "F"
            If Data(0x38) = 65 Then    ' "A"
              If Data(0x39) = 84 Then  ' "T"
                TensDig = Data(0x3A) - 48
                UnsDig = Data(0x3B) - 48
                FatType = TensDig * 10 + UnsDig
                If FatType = 12 Then FAT12 = True
                If FatType = 16 Then FAT16 = True
                If FatType = 32 Then FAT32 = True
              End If
            End If

          End If

        End If

      End If

    End If

    #IFDEF Debug_SD_ReadBlock
      If recycle = 1 Then
        Call SD_BlockHeader(StartAddr)
      End If
    #ENDIF

    If recycle = 4 AND SD_BSE = False Then
      'determine FAT type, get BSEAddr, etc.
      Call SD_PartTblVal
    End If

    If StartAddr = 0 OR StartAddr = BSEBlkAddr Then
      If recycle = 1 AND SD_BSE = True Then
          Call SD_GetRootDir
      End If
    End If

    arg = 0
    'parse 128 Byte array into Qty 8 x 16 Byte lines
    For ByteX16 = 0 to 7

      #IFDEF Debug_SD_ReadBlock
        HSerPrint Hex (count):HSerPrint "0"
      #ENDIF

      count += 1
      For count1 = 0 to 15
        arg += 1
        #IFDEF Debug_SD_ReadBlock
          HSerPrint " ":HSerPrint HEX (Data(arg))
        #ENDIF

      Next

      #IFDEF Debug_SD_ReadBlock
        HSerPrint "  "
      #ENDIF

      'reset data array row and print ASCII characters
      arg = arg - 16
      For count2 = 0 to 15

        arg += 1
        'HSerPrint " "
        'check for null value
        PrintValue = False
        SpaceValue = Data(arg)
        If SpaceValue < 32 OR SpaceValue > 126 then PrintValue = True

        #IFDEF Debug_SD_ReadBlock
          If PrintValue = True Then
            HSerPrint "."
          Else
            HSerPrint CHR (Data(arg))
          End If
        #ENDIF

      Next

      'express file data only in Root Directory
      If StartAddr = RootDIRAddr OR StartAddr = MoreDIRSectors Then

        'parse second line of file entry if filename valid
        If FileAttr = True Then   'every odd line
          Call SD_FileAttributes
          FileAttr = False
        End If

        'parse first line of file entries for valid filename
        If ByteX16.0 = 0 Then   'every even line
            Call SD_FileName
        End If
      End If

      #IFDEF Debug_SD_ReadBlock
        If arg = 16 Then HSerPrintCRLF
        If arg = 32 Then HSerPrintCRLF
        If arg = 48 Then HSerPrintCRLF
        If arg = 64 Then HSerPrintCRLF
        If arg = 80 Then HSerPrintCRLF
        If arg = 96 Then HSerPrintCRLF
        If arg = 112 Then HSerPrintCRLF
        If arg = 128 Then HSerPrintCRLF
      #EndIf

    Next

  Next

  SPITransfer (0xFF, SDresponse)    'Receive 16bit CRC
  SPITransfer (0xFF, SDresponse)

  #IFDEF Debug_SD_ReadBlock
    HSerPrintCRLF
  #ENDIF

  Set SD_CS On

end sub


sub SD_PartTblVal

'HSerPrint "Hello"

  'HSerPrint "parttbl":HSerPrint " "
  FatType = Data(67)
  'HSerPrint Data(67):HSerPrintCRLF
  If FatType = 1 Then FAT12 = True
  If FatType = 4 OR FatType = 6 Then FAT16 = True
  If FatType = 14 Then FAT16 = True
  If FatType = 11 OR FatType = 12 Then FAT32 = True
  'HSerPrint "(0x"
  'HSerPrint Hex (FatType):HSerPrint ")"
  'HSerPrintCRLF

  If SD_SDHC = False OR FAT16 = True Then
    'FAT16 is Byte based addr
    BSEBlkAddr = BSEBlkAddr * 512
  End If

  BSEBlkAddr = Data(71)
  BSEBlkAddr_H = Data(72)
  BSEBlkAddr_U = Data(73)
  BSEBlkAddr_E = Data(74)



'  HSerPrint "(0x"
'  HSerPrint Hex (BSEBlkAddr_E)
'  HSerPrint Hex (BSEBlkAddr_U)
'  HSerPrint Hex (BSEBlkAddr_H)
'  HSerPrint Hex (BSEBlkAddr)
'  HSerPrint ") "
'  HSerPrintCRLF

end sub


sub SD_FileAttributes

Dim fileyear, cluster, as Word
Dim ByteOffset, StartCluster as Long

  'date Bytes 0x18, 0x19
  #IFDEF Debug_SD_FileAttributes
    HSerPrint " "
    year = arg - 6
    fileyear = Data(year) AND 0xFE
    Rotate fileyear Right Simple
    fileyear = fileyear + 1980
    HSerPrint fileyear:HSerPrint "/"

    mthH = Data(year) AND 0x01
    mthH = mthH * 8
    mth = arg - 7
    mthL = Data(mth) AND 0xE0
    mthL = mthL / 32
    filemonth = mthH + mthL
    HSerPrint filemonth:HSerPrint "/"

    fileday = Data(mth) AND 0x1F
    HSerPrint fileday:HSerPrint " "

    'time Bytes 0x16, 0x17
    hours = arg - 8
    filehrs = Data(hours) AND 0xF8
    filehrs = filehrs / 8
    HSerPrint filehrs:HSerPrint ":"

    minutesH = Data(hours) AND 0x07
    minutesH = minutesH * 8
    mins = arg - 9
    minutesL = Data(mins) AND 0xE0
    minutesL = minutesL / 32
    minutes = minutesH + minutesL
    HSerPrint minutes:HSerPrint ":"

    secs = Data(mins) AND 0x1F
    Rotate secs Left Simple
    HSerPrint secs

    If Fat32 = True OR SD_SDHC = True Then HSerPrint " Sector"
    If Fat16 = True AND SD_SDHC = False Then HSerPrint " Byte"

    HSerPrint " Offset 0x"
  #ENDIF
  'print and record cluster

If FAT32 = True Then
  'File Folder Start Cluster (Low) + (High) RootDIR number 0x14 and 0x15
  cluster = arg - 5
  StartCluster = Data(cluster)
  cluster = arg - 4
  StartCluster_H = Data(cluster)
  cluster = arg - 11
  StartCluster_U = Data(cluster)
  cluster = arg - 10
  StartCluster_E = Data(cluster)
Else
  'File Folder Start Cluster (Low) RootDIR number 0x1A and 0x1B
  cluster = arg - 5
  StartCluster = Data(cluster)
  cluster = arg - 4
  StartCluster_H = Data(cluster)
End If
  'SectorsPerCluster from BSE and SD_GetRootDir
  'BytesPerSector defined as 512
  StartCluster = (StartCluster - 2) * SectorsPerCluster
    'HSerPrint HEX (StartCluster_E)
    'HSerPrint HEX (StartCluster_U)
    'HSerPrint HEX (StartCluster_H)
    'HSerPrint HEX (StartCluster):HSerPrint " "

    'HSerPrint HEX (SectorsPerCluster):HSerPrint " "

  If FAT32 = True Then
    FileDataAddr = RootDirAddr + StartCluster
  End If

  If FAT16 = True Then

    If SD_SDHC = True Then
      'MaxRootEntries = 32 (sectors)
      FileDataAddr = RootDIRAddr + 32 + StartCluster
    Else
      'MaxRootEntries defined as 512*32
      FileDataAddr = RootDIRAddr + MaxRootEntries
      FileDataAddr = FileDataAddr + (StartCluster * BytesPerSector)
    End If

  End If

  'Print and store file address offset location
  #IFDEF Debug_SD_FileAttributes
    HSerPrint Hex (FileDataAddr_E)
  #ENDIF
  LastEEAddr += 1
  EPWrite (LastEEAddr, FileDataAddr_E)

  #IFDEF Debug_SD_FileAttributes
    HSerPrint Hex (FileDataAddr_U)
  #ENDIF
  LastEEAddr += 1
  EPWrite (LastEEAddr, FileDataAddr_U)

  #IFDEF Debug_SD_FileAttributes
    HSerPrint Hex (FileDataAddr_H)
  #ENDIF
  LastEEAddr += 1
  EPWrite (LastEEAddr, FileDataAddr_H)

  #IFDEF Debug_SD_FileAttributes
    HSerPrint Hex (FileDataAddr)
  #ENDIF
  LastEEAddr += 1
  EPWrite (LastEEAddr, FileDataAddr)

  #IFDEF Debug_SD_FileAttributes
    HSerPrint " File Size(Bytes) = 0x"
  #ENDIF

  'File Size in Bytes 0x1C to 0x1F
  size = arg - 3
  ByteOffset = Data(size)
  size = arg - 2
  ByteOffset_H = Data(size)
  size = arg - 1
  ByteOffset_U = Data(size)
  size = arg
  ByteOffset_E = Data(size)

  #IFDEF Debug_SD_FileAttributes
    HSerPrint Hex (ByteOffset_E)
    HSerPrint Hex (ByteOffset_U)
    HSerPrint Hex (ByteOffset_H)
    HSerPrint Hex (ByteOffset)
  #ENDIF

end sub


sub SD_FileName
  'decode first eight bytes of file name and 3 bytes of file type
  'printfile = False
  'tilde = False

  'test valid characters for each file name pass
  'exit sub when any of first eight char's not valid
  'valid char's: alpha lower and upper Case
  'numerals, tildes, and spaces

  'keep track of sector with last record entry
  'valid entries will not have first byte as zero
  chrs = arg - 15
  If Data(chrs) = 0 Then
   LastRecord = True
  End If

  'check  attribute Byte 0x0B
  'bits 6 and 7 should always be zero
  '0XFF means sd init failed or bad sector address
  chrs = arg - 4
  If Data(chrs) = 0xFF Then LastRecord = True

  For letter = 15 to 5

    printfile = False
    tilde = False
    'arg is the Byte placeholder for each Block read pass(s)
    chrs = arg - letter
    name = Data(chrs)

    'Upper case letters
    If name >= 65  AND name <= 90 Then
      printfile = True
    End If

    'lower case letters
    If name >= 97  AND name <= 122 Then
      printfile = True
    End If

    'space and tilde
    If name = 32 OR name = 126 Then
      If name = 126 Then tilde = True
      printfile = True
    End If

    'numerals
    If name >= 48 AND name <= 57 Then
      tilde = True then
      printfile = True
    End If

    'no valid char encountered during first 11 Bytes of file name
    If printfile = False Then
        'HSerPrint "exit"
        exit sub
    End If

  Next

  chrs = arg - 4
  'print only archive or archive read only files
  If Data(chrs) = 0x20 OR Data(chrs) = 0x21 Then
    printfile = True
  Else
    printfile = False
  End If


  If printfile = True Then
    'eeFile = True
    #IFDEF Debug_SD_FileAttributes
      HSerPrint " "
    #ENDIF
    SD_NumFiles += 1

    'record ASCII filename less file extension to eeprom
    'additional four bytes will be added for file location
    'for a total of 12 bytes per file record
    For letter = 15 to 8
      LastEEAddr += 1
      chrs = arg - letter
      name = Data(chrs)
      EPWrite LastEEAddr, name
    Next

    For letter = 15 to 5
      chrs = arg - letter
      name = Data(chrs)
      'EPWrite chrs, name
      If letter = 7 Then
        #IFDEF Debug_SD_FileAttributes
          HserPrint "."
        #ENDIF
        eeFile = False

      End If

      If name >= 65  AND name <= 90 Then
        #IFDEF Debug_SD_FileAttributes
          HSerPrint CHR (name)
        #ENDIF
      End If

      If name >= 97  AND name <= 122 Then
        #IFDEF Debug_SD_FileAttributes
          HSerPrint CHR (name)
        #ENDIF
      End If

      If name = 126 Then
        #IFDEF Debug_SD_FileAttributes
          HSerPrint CHR (name)
        #ENDIF
      End If

      If name >= 48 AND name <= 57 Then
        #IFDEF Debug_SD_FileAttributes
          HSerPrint CHR (name)
        #ENDIF
      End If

    Next

    FileAttr = True

  End If

end sub


sub SD_GetRootDir
Dim ReservedBlocks as Word

  'Boot Sector Registers 0x0D for later calc's
  SectorsPerCluster = Data(14)
  'HSerPrint "Sectors Per Cluster 0x"
  'HSerPrint Hex (SectorsPerCluster):HSerPrintCRLF

  'BSE 0x0E and 0x0F
  ReservedBlocks = Data(15)
  ReservedBlocks_H = Data(16)

  'HSerPrint "ReservedBlocks 0x"
  'HSerPrint Hex (ReservedBlocks_H)
  'HSerPrint Hex (ReservedBlocks)
  'HSerPrintCRLF

  'BSE 0x10 always 0x02
  NumFAT = Data(17)
  'HSerPrint "NumFAT 0x"
  'HSerPrint Hex (NumFAT):HSerPrintCRLF

  'If FAT16 = True OR SD_SDHC = True Then
  If FAT16 = True Then
    'BSE 0x16 and 0x17
    SizeFAT = Data(23)
    SizeFAT_H = Data(24)
  Else
    'BSE 0x24 and 0x25
    SizeFAT = Data(37)
    SizeFAT_H = Data(38)
    SizeFAT_U = Data(39)
    SizeFAT_E = Data(40)
  End If

'  HSerPrint "SizeFAT 0x"
'  HSerPrint HEX (SizeFAT_E)
'  HSerPrint HEX (SizeFAT_U)
'  HSerPrint HEX (SizeFAT_H)
'  HSerPrint HEX (SizeFAT)
'  HSerPrintCRLF

  'FAT32 OR SDHC cards deal in Sectors NOT Bytes like FAT16
  'so no further adjustment to RootDirAddr
  If FAT32 = True OR SD_SDHC = True Then
    RootDIRAddr = (SizeFAT * NumFAT) + ReservedBlocks  + BSEBlkAddr   '+ HiddenRegisters
  Else
    'FAT16 deals with offset in Bytes
    RootDIRAddr = ((SizeFAT * NumFAT) + ReservedBlocks) * BytesPerSector
    RootDIRAddr = RootDIRAddr + BSEBlkAddr
  End If

  'HSerPrint "RootDirAddr 0x"
  'HSerPrint Hex (RootDIRAddr_E)
  'HSerPrint Hex (RootDIRAddr_U)
  'HSerPrint Hex (RootDIRAddr_H)
  'HSerPrint Hex (RootDIRAddr)
  'HSerPrintCRLF

end sub



sub SD_WordBlockHeader(OffsetAddr)

  HSerPrintCRLF

  'If OffsetAddr > RootDirAddr And FileStartAddr = False Then exit sub
  If OffsetAddr = 0 Then HSerPrint "MBR"
  If OffsetAddr = BSEBlkAddr Then HSerPrint "BSE"

  If OffsetAddr = RootDIRAddr OR OffsetAddr = MoreDIRSectors Then
    HSerPrint "Root Directory"
  End If

  If OffsetAddr > RootDIRAddr AND FileStartAddr = True Then
    HSerPrint "File Data"

  End If


  HSerPrint " Address - 0x"
  HSerPrint Hex (OffsetAddr_E):HSerPrint Hex (OffsetAddr_U)
  HSerPrint Hex (OffsetAddr_H):HSerPrint Hex (OffsetAddr)


  HSerPrintCRLF
  If OffsetAddr > RootDirAddr And FileStartAddr = False Then exit sub

  HSerPrint "       0    1    2    3    4    5    6    7    8    9    a    b    c    d    e    f"
  HSerprintCRLF

end sub

sub SD_BlockHeader(OffsetAddr)

  'HSerPrintCRLF
  'If OffsetAddr = 0 Then HSerPrint "MBR"

  If OffsetAddr = 0 Then
    If SD_MBR = True Then
      HSerPrint "MBR"
    Else
      HSerPrint "BSE"
    End If
  End If

  If OffsetAddr = BSEBlkAddr Then HSerPrint "BSE"

  If OffsetAddr = RootDIRAddr OR OffsetAddr = MoreDIRSectors Then
    HSerPrint "Root Directory"
  End If

  If OffsetAddr > RootDIRAddr And LastRecord = True Then
    HSerPrint "File Data"
  End If


  HSerPrint " Address - 0x"
  HSerPrint Hex (OffsetAddr_E):HSerPrint Hex (OffsetAddr_U)
  HSerPrint Hex (OffsetAddr_H):HSerPrint Hex (OffsetAddr)
  HSerPrintCRLF
  HSerPrint "     0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f"
  HSerprintCRLF

end sub

sub SD_ManfID (IN ManID)
  Select Case ManID

    Case 1
      HSerPrint "Panasonic"
    Case 2
      HSerPrint "Toshiba/Kingston/Viking"
    Case 3
      HSerPrint "SanDisk"
    Case 8
      HSerPrint "Silicon Power"
    Case 24   '0x18
      HSerPrint "Infineon"
    Case 27   '0x1B
      HSerPrint "Samsung/Transcend"
    Case 28   '0x1C
      HSerPrint "Transcend"
    Case 29   '0x1D
      HSerPrint "Corsair"
    Case 30   '0x1E
      HSerPrint "Transcend"
    Case 31   '0x1F
      HSerPrint "Kingston"
    Case 39   '0x27
      HSerPrint "Sony/Delkin/PNY/Patriot"
    Case 40   '0x28
      HSerPrint "Lexar/PNY"
    Case 48   '0x30
      HSerPrint "SanDisk"
    Case 51   '0x33
      HSerPrint "STMicroElectronics"
    Case 65   '0x41
      HSerPrint "Kingston"
    Case 101  '0x6F
      HSerPrint "STMicroElectronics"
    Case 116  '0x74
      HSerPrint "Kingston/Transcend"
    Case 118  '0x76
      HSerPrint "Patriot"
    Case 130  '0x82
      HSerPrint "Sony"
    Case Else
      HSerPrint "Unknown"

  End Select
end sub


sub SD_DateMo(In Mth as byte)

  Select Case Mth
    Case 1
      HSerPrint "JAN"
    Case 2
      HSerPrint "FEB"
    Case 3
      HSerPrint "MAR"
    Case 4
      HSerPrint "APR"
    Case 5
      HSerPrint "MAY"
    Case 6
      HSerPrint "JUN"
    Case 7
      HSerPrint "JUL"
    Case 8
      HSerPrint "AUG"
    Case 9
      HSerPrint "SEP"
    Case 10
      HSerPrint "OCT"
    Case 11
      HSerPrint "NOV"
    Case 12
      HSerPrint "DEC"
  End Select

End sub
