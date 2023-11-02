*-----------------------------------------------------------------------------
* <Rating>-32</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE AT.POST.CHG.FT.BAL(ISO.RESPONSE)
*-----------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT T24.BP I_GTS.COMMON
    $INSERT T24.BP I_F.OFS.SOURCE
    $INSERT T24.BP I_F.FT.COMMISSION.TYPE
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.ALTERNATE.ACCOUNT
    $INCLUDE ATM.BP I_F.ATM.PARAMETER
    $INCLUDE ATM.BP I_F.ATM.CHARGE.TABLE
    $INCLUDE ATM.BP I_AT.ISO.COMMON
    $INCLUDE ATM.BP I_F.ATM.REVERSAL
    $INCLUDE ATM.BP I_F.INTRF.MAPPING
**-----------------------------------------------------------------------------
* Description : This routine will do charge for the following transactions
**-----------------------------------------------------------------------------
*      1. Balance Enquiry - our bank Customer doing transaction at other ATM
**-----------------------------------------------------------------------------
* Revision History
*
*    Date         Pacs-Id           Changed By            Description
*    -------------------------------------------------------------------------
*  23-7-2020     nuha tuhul            add account card holder AND add MTI 0100
*-------------------------------------------------------------------------------
** intial methods*******

    GOSUB CHECK.PRELIM.COND
    IF PROCESS.GOAHEAD THEN
        GOSUB OPEN.FILE
        GOSUB PROCESS
    END

    RETURN

*-----------------------------------------------------------------------------
CHECK.PRELIM.COND:
*-----------------------------------------------------------------------------
    PROCESS.GOAHEAD = '1'

    MAX.LOOP.CNT = 4
    LOOP.CNT = 1

    LOOP
    WHILE LOOP.CNT LE MAX.LOOP.CNT AND PROCESS.GOAHEAD DO

        BEGIN CASE

        CASE LOOP.CNT EQ 1
            FN.ATM.PARAMETER = 'F.ATM.PARAMETER'
            F.ATM.PARAMETER = ''
            CALL OPF(FN.ATM.PARAMETER,F.ATM.PARAMETER)
            CALL CACHE.READ(FN.ATM.PARAMETER,'SYSTEM',R.ATM.PARAMETER,E.ATM.PARAMETER)

            CO.CODE = R.ATM.PARAMETER<ATM.PARA.VISA.DEPT>
            OUR.BIN = R.ATM.PARAMETER<ATM.PARA.BANK.IMD>
            BIN.NO = AT$INCOMING.ISO.REQ(32)
            LOCATE BIN.NO IN OUR.BIN<1,1> SETTING POS THEN
**            IF BIN.NO EQ OUR.BIN THEN
                PROCESS.GOAHEAD = 0
            END

        CASE LOOP.CNT EQ 2
            OFS.RES = FIELD(ISO.RESPONSE,'Y.ISO.RESPONSE:1:1=',2)

            IF OFS.RES[1,2] NE '00' THEN
                PROCESS.GOAHEAD = '0'
            END

        CASE LOOP.CNT EQ 3
            PROCESSING.CODE=AT$INCOMING.ISO.REQ(3)
            IF PROCESSING.CODE[1,2] NE '30' THEN
                PROCESS.GOAHEAD = '0'
            END

        CASE LOOP.CNT EQ 4

            NETWORK.TYPE = AT$INCOMING.ISO.REQ(48)
            NETWORK.TYPE = TRIM(NETWORK.TYPE," ","B")

            IF NETWORK.TYPE EQ '' THEN
                PROCESS.GOAHEAD = 0
            END
        END CASE

        LOOP.CNT += 1

    REPEAT

    RETURN
*-----------------------------------------------------------------------------
OPEN.FILE:
*-----------------------------------------------------------------------------

    FN.ATM.CHARGE.TABLE = 'F.ATM.CHARGE.TABLE'
    F.ATM.CHARGE.TABLE = ''
    CALL OPF(FN.ATM.CHARGE.TABLE,F.ATM.CHARGE.TABLE)

    FN.FT.COMM.TYPE = 'FBNK.FT.COMMISSION.TYPE'
    F.FT.COMM.TYPE = ''
**    CALL OPF(FN.FT.COMM.TYPE,F.FT.COMM.TYPE)
    OPEN FN.FT.COMM.TYPE TO F.FT.COMM.TYPE ELSE
        CRT "ERROR IN OPEN"
    END

    FN.FUNDS.TRANSFER = 'FBNK.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
**    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)
    OPEN FN.FUNDS.TRANSFER TO F.FUNDS.TRANSFER ELSE
        CRT "ERROR IN OPEN"
    END
    LCY.ID="JOD"

    FN.INTRF.MAPPING = 'F.INTRF.MAPPING'
    F.INTRF.MAPPING =  ''
    CALL OPF(FN.INTRF.MAPPING,F.INTRF.MAPPING)

    FN.NUMERIC.CURRENCY = 'F.NUMERIC.CURRENCY'
    F.NUMERIC.CURRENCY = ''
    CALL OPF(FN.NUMERIC.CURRENCY,F.NUMERIC.CURRENCY)

    PROCESSING.CODE=AT$INCOMING.ISO.REQ(3)
    PROC.CODE = PROCESSING.CODE[1,2]
    MAP.ID = AT$AT.REQ.MAP.ID
    TERM.ID = AT$INCOMING.ISO.REQ(41)
    MTI = AT$AT.REQ.MAP.ID[4,4]
    CO.CODE = R.ATM.PARAMETER<ATM.PARA.VISA.DEPT>


    CALL CACHE.READ(FN.INTRF.MAPPING,MAP.ID,R.INTRF.MAPPING,E.INTRF.MAPPING)
    IF R.INTRF.MAPPING THEN
        USERNAME = R.INTRF.MAPPING<INTRF.MAP.OFS.USER>
        PASSWORD = R.INTRF.MAPPING<INTRF.MAP.OFS.PASSWORD>
    END

    AT.REV.ID = AT$INCOMING.ISO.REQ(11):AT$INCOMING.ISO.REQ(7):AT$INCOMING.ISO.REQ(32):AT$INCOMING.ISO.REQ(37)

    FN.ACCOUNT = 'FBNK.ACCOUNT'
    F.ACCOUNT = ''
**    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    OPEN FN.ACCOUNT TO F.ACCOUNT ELSE
        CRT "ERROR IN OPEN"
    END

    FN.ALT.ACCOUNT="FBNK.ALTERNATE.ACCOUNT"
    FV.ALT.ACCOUNT=""
**    CALL OPF(FN.ALT.ACCOUNT,FV.ALT.ACCOUNT)
    OPEN FN.ALT.ACCOUNT TO  FV.ALT.ACCOUNT ELSE
        CRT "ERROR IN OPEN"
    END


    RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
    MTI.CODE = AT$AT.REQ.MAP.ID[4,4]
*    NETWORK.TYPE = "NETWORK.":NETWORK.TYPE
    NETWORK.TYPE = NETWORK.TYPE:MTI.CODE

    R.ATM.CHARGE.TABLE = ''

    CALL CACHE.READ(FN.ATM.CHARGE.TABLE,NETWORK.TYPE,R.ATM.CHARGE.TABLE,E.ATM.CHARGE.TABLE)

    IF R.ATM.CHARGE.TABLE EQ '' THEN
        RETURN
    END

    COMM.TYPE = ''
    COMM.AMT = ''
    TXN.CUR = LCY.ID
    FINDSTR PROC.CODE IN R.ATM.CHARGE.TABLE<AT.CHG.PROC.CODE> SETTING PROC.CODE.FM.POS,PROC.CODE.VM.POS THEN
        COMM.TYPE = R.ATM.CHARGE.TABLE<AT.CHG.COMM.TYPE,PROC.CODE.VM.POS>
        BEGIN CASE
        CASE R.ATM.CHARGE.TABLE<AT.CHG.COMM.AMT,PROC.CODE.VM.POS,1> NE ''
            COMM.AMT = R.ATM.CHARGE.TABLE<AT.CHG.COMM.AMT,PROC.CODE.VM.POS,1>
            TXN.CUR = COMM.AMT[1,3]
            COMM.AMT = COMM.AMT[4,LEN(COMM.AMT)-1]
        CASE R.ATM.CHARGE.TABLE<AT.CHG.COMM.PERC,PROC.CODE.VM.POS,1> NE ''
            COMM.PERC = R.ATM.CHARGE.TABLE<AT.CHG.COMM.PERC,PROC.CODE.VM.POS,1>
            TXN.AMT = ''
            TXN.AMT = AT$INCOMING.ISO.REQ(4)
            IF TXN.AMT NE '' THEN
                TXN.AMT = TRIM(TXN.AMT," ","L")
                TXN.AMT = TXN.AMT/1000
                COMM.AMT = (TXN.AMT * COMM.PERC) / 100
            END
        CASE R.ATM.CHARGE.TABLE<AT.CHG.INCOMING.VALUE,PROC.CODE.VM.POS,1> NE ''
            FLD.MSG.POSN = R.ATM.CHARGE.TABLE<AT.CHG.INCOMING.VALUE,PROC.CODE.VM.POS,1>
            YR.FIELD = ''
            YR.FIELD.POSN = ''
            COMM.CCY = FIELD(FLD.MSG.POSN,'*',1)
            COMM.CCY = COMM.CCY[2,999]
*            IF COMM.CCY EQ '' THEN
            COMM.CCY = AT$INCOMING.ISO.REQ(51)
*            END
            IF NUM(COMM.CCY) THEN
                GOSUB GET.CURRENCY
            END
            YR.FIELD.POSN = FIELD(FLD.MSG.POSN,'*',2)
            YR.FIELD = AT$INCOMING.ISO.REQ(YR.FIELD.POSN)
            COMM.DETS = COMM.CCY:'%':YR.FIELD
            CALL AT.GET.COM.AMT(COMM.DETS,COMM.AMT)
            COMM.AMT = COMM.AMT[4,999]
        CASE OTHERWISE
            RETURN

        END CASE
    END ELSE
        RETURN
    END

    AT.DEBIT.ACCT = AT$INCOMING.ISO.REQ(102)

    CALL CACHE.READ(FN.FT.COMM.TYPE,COMM.TYPE,R.FT.COMM.TYPE,R.FT.COMM.TYPE.ERR)
    CREDIT.ACCT = R.FT.COMM.TYPE<FT4.CATEGORY.ACCOUNT>
    CREDIT.ACCT = CREDIT.ACCT[1,12]:CO.CODE[6,4]
    TERM.DET = R.FT.COMM.TYPE<FT4.DESCRIPTION>
    CREDIT.AMOUNT = COMM.AMT

    IF LEN(CREDIT.ACCT) EQ 5 THEN
        CREDIT.ACCT = "PL":CREDIT.ACCT
    END

    YR.AC.CARD.HOLDER = ''
    YR.AC.CARD.HOLDER = AT$INCOMING.ISO.REQ(103)

    IF MTI EQ '0200' OR MTI EQ '0220' OR MTI EQ '0100' THEN
        OFS.MSG = 'FUNDS.TRANSFER,ATM.CHG/I/PROCESS,':USERNAME:'/':PASSWORD:'/':CO.CODE:',,'
        OFS.MSG:= 'TRANSACTION.TYPE::=ACBL,DEBIT.ACCT.NO::=':AT.DEBIT.ACCT:','
        OFS.MSG:= 'CREDIT.CURRENCY::=':LCY.ID:',CREDIT.AMOUNT::=':CREDIT.AMOUNT:','
*        OFS.MSG:= 'DEBIT.CURRENCY::=':COMM.CCY:',DEBIT.AMOUNT::=':CREDIT.AMOUNT:','
        OFS.MSG:= 'CREDIT.ACCT.NO::=':CREDIT.ACCT:','
*        OFS.MSG:= 'DEBIT.THEIR.REF::=':TERM.DET:',':'CREDIT.THEIR.REF::=':TERM.DET:','
        OFS.MSG:= 'DEBIT.THEIR.REF::=':YR.AC.CARD.HOLDER:',':'CREDIT.THEIR.REF::=':YR.AC.CARD.HOLDER:','
        OFS.MSG:= 'AT.UNIQUE.ID:: =':AT.REV.ID
    END ELSE
        INCOMING=TERM.ID:"%":AT.REV.ID
        CALL AT.ISO.REV(INCOMING,REV.FT.ID)
        OFS.MSG = 'FUNDS.TRANSFER,ATM.CHG/R/PROCESS,':USERNAME:'/':PASSWORD:'/,':REV.FT.ID
    END

    CALL RAD.LOG.MSG("ATM","DEBUG","The Charge Transaction Req Msg. : ":OFS.MSG)
    CALL OFS.PROCESS.MANAGER(OFS.MSG,OFS.RESP)
    CALL RAD.LOG.MSG("ATM","DEBUG","Charges Transaction Resp Msg : ":OFS.RESP)

    GOSUB CALC.BAL.AFT.CHG

    RETURN

*----------------------------------------------------------------------*
GET.UNIQUE.ID:
*----------------------------------------------------------------------*
    CALL ALLOCATE.UNIQUE.TIME(UNIQUE.TIME)
    CHANGE '.' TO '' IN UNIQUE.TIME
    LEN.UNIQUE.TIME = LEN(UNIQUE.TIME) -6
    Y.UNIQUE.ID = UNIQUE.TIME[LEN.UNIQUE.TIME,6]

    RETURN
*----------------------------------------------------------------------*
CALC.BAL.AFT.CHG:
*----------------------------------------------------------------------*

    ERR.FLAG = FIELD(FIELD(OFS.RESP,',',1,1),'/',3,1)

    IF ERR.FLAG EQ '1' THEN
        Y.ACCT.NO = AT.DEBIT.ACCT
        CACHE.OFF =1
        GOSUB READ.ACCT
        CACHE.OFF = 0
    END ELSE
        RETURN
    END

    IF R.ACCT EQ '' THEN
        RETURN
    END

    GOSUB GET.UNIQUE.ID

    FLD.RTN = R.INTRF.MAPPING<INTRF.MAP.PRE.RTN>
    COMP.FLAG= ''
    CALL CHECK.ROUTINE.EXIST(FLD.RTN,COMP.FLAG,RET.INFO)

    IF COMP.FLAG EQ '1' THEN
        BALANCE.FORMATTED = ''
        SAVE.FLD.32 = ''
        SAVE.FLD.32 = AT$INCOMING.ISO.REQ(32)
        AT$INCOMING.ISO.REQ(32) = OUR.BIN
        CALL @FLD.RTN(R.ACCT,BALANCE.FORMATTED,FUTURE.USE)
        AT$INCOMING.ISO.REQ(32) = SAVE.FLD.32
        CALL RAD.LOG.MSG("ATM","DEBUG","Calling ":FLD.RTN :" ,":FLD.VALUE:" ,":RET.VALUE)
        BALANCE.FORMATTED.LEN=LEN(BALANCE.FORMATTED)
        BALANCE.FORMATTED.LEN.FMT=FMT(BALANCE.FORMATTED.LEN,'R%3')
        ISO.RESPONSE = 'BALANCE:1:1=':BALANCE.FORMATTED:',':'UNIQUE.TXN.CODE:1:1=':Y.UNIQUE.ID:',Y.ISO.RESPONSE:1:1=00'
    END

    RETURN
*----------------------------------------------------------------------*
READ.ACCT:
*----------------------------------------------------------------------*

    R.ACCT = ''

    CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCT,F.ACCOUNT,AC.ER)

    IF NOT(R.ACCT) THEN
        CALL F.READ(FN.ALT.ACCOUNT,Y.ACCT.NO,R.AT.ACCT,FV.ALT.ACCOUNT,ALT.ERR)
        Y.ACCT.NO=R.AT.ACCT<AAC.GLOBUS.ACCT.NUMBER>
        CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCT,F.ACCOUNT,AC.ER)
    END

    RETURN
*----------------------------------------------------------------------*
GET.CURRENCY:
*----------------------------------------------------------------------*

    R.NUMERIC.CCY = ''
    NUM.ERR = ''
    CALL F.READ(FN.NUMERIC.CURRENCY,COMM.CCY,R.NUMERIC.CCY,F.NUMERIC.CURRENCY,NUM.ERR)
    IF R.NUMERIC.CCY THEN
        COMM.CCY = R.NUMERIC.CCY<1>
    END

    RETURN

*----------------------------------------------------------------------*
END
