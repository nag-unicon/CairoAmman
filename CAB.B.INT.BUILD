*-----------------------------------------------------------------------------
* <Rating>-419</Rating>
*----------------------------------------------------------------------------------------------------------------------------------------------------
    SUBROUTINE CAB.B.INT.BUILD(IN.ID.MASK)
*-------------------------------------------------------------------------------------------------------------------------------------
*Company   Name    : Cairo Amman Bank
*Developed By      : Temenos Application Management
*Program   Name    : CAB.VAL.UPD.CHARGES
*---------------------------------------------------------------
*Linked With       : BATCH>BNK/CAB.B.INT.BUILD
*In  Parameter     : N/A
*Out Parameter     : N/A
*ODR number used   : ODR-2011-10-0066
*Description       :
* This is COB routine to get the average balance of an account and its sub accounts.
* The interest is calculated for each account and the details are written in the live file
* CAB.INTACCT.INT.ACCR. The sum of interest for all sub accounts and its corresponding master
* account is calculated and entry is raised.
* This live file will be maintanied for only interbranch transactions
* handled by means of internal accounts belonging to a particular category.
* Incoming Arguement: IN.ID.MASK
* Outgoing Arguement:
*-----------------------------------------------------------------------------------------------------------------------------------------------
* Modification History
*---------------------
*23-Nov-12 - ODR-2012-11-0042 - Change in forming start date and end date
*21-May-13 - RTC_673069 -  Change in currency converison
*--------------------------------------------------------------
* Insert files:
*-------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCT.ACTIVITY
    $INSERT I_F.ACCOUNT
    $INSERT I_GTS.COMMON
    $INSERT I_F.OFS.SOURCE
    $INSERT I_F.COMPANY
    $INSERT I_F.CURRENCY
    $INSERT I_F.INTEREST.BASIS
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_BATCH.FILES
    $INCLUDE CAB.BP I_F.CAB.INTACCT.INT.ACCR
    $INCLUDE CAB.BP I_F.CAB.CCY.INT.RATE
    $INCLUDE CAB.BP I_F.CAB.L.INT.BUILD
    $INCLUDE CAB.BP I_CAB.B.INT.BUILD.COMMON

    GOSUB INIT
    GOSUB PROCESS

    RETURN
*-----------------------------------------------------------------------------------------------------------------------------------------------------------
INIT:
*----
    YFLAG = ''
    Y.CCY.ID = ''
    Y.CCY.ARR = ''
    Y.NO.OF.DAYS = ''
    Y.TOT.CR.INT.AMT = ''
    Y.TOT.DR.INT.AMT = ''
    Y.PL.DEBIT.CATEG = ''
    Y.PL.CREDIT.CATEG = ''
    Y.WASH.ACCT = ''
    Y.OFS.SRC.ID = ''
    FINAL.NET.AMT = ''
    LCY.AMOUNT = ''
    FCY.AMOUNT = ''
    Y.EXCH.RATE = ''
    Y.HO.COMPANY = ''
    ST.CCY = ''
    Y.POST.CCY = ''
    Y.CO.CODE = ''
    Y.CATEGORY = ''
    Y.ACCOUNT.OFFICER = ''
    Y.CUST.NO = ''
    Y.AC.OPEN.DATE = ''
    ACCT.AVG.BAL = ''
    Y.ACCOUNT.ID = ''
    POS = ''
    Y.ACCOUNT.ARR = ''

    RETURN
*----------------------------------------------------------------------------------------------------------------------------------
PROCESS:
*-------

    CURR.APP.UPD =  FIELD(IN.ID.MASK,'_?_',1)
    Y.ID = FIELD(IN.ID.MASK,'_?_',2)

    Y.START.DATE = ''
    Y.START.DATE = TODAY

    DAYS = "1W"
    REGION = ""
    CALL CDT(REGION,Y.START.DATE,DAYS)

*ODR-2012-11-0042-S
    Y.ONE = '01'
    Y.STRT.MN = Y.START.DATE[5,2]
    IF Y.STRT.MN EQ Y.ONE THEN
        Y.MNTH = '12':Y.ONE
        START.DATE = Y.START.DATE[1,4] -1:Y.MNTH
    END ELSE
        START.DATE = Y.START.DATE[1,6] -1:Y.ONE
    END
*    YY.END.DATE = TODAY[1,6]:Y.ONE
    YY.END.DATE = Y.START.DATE[1,6]:Y.ONE
    DAYS = "-1C"
    REGION = ""
    CALL CDT(REGION,YY.END.DATE,DAYS)
    END.DATE = YY.END.DATE
*ODR-2012-11-0042-E

    Y.VALUE.DATE = YY.END.DATE
    Y.PROCESS.DATE = YY.END.DATE

    Y.ACCT.ACTIVITY.ID = Y.ID:'-':START.DATE[1,6]
    Y.MASTER.ACCOUNT = ''
    Y.MASTER.ACCOUNT = Y.ID
    GOSUB SUB.ACCOUNT.PROCESS

    R.CAB.INTACCT.INT.ACCR = ''
    ERR.CAB.INT = ''
    CALL F.READ(FN.CAB.INTACCT.INT.ACCR,Y.ACCT.ACTIVITY.ID,R.CAB.INTACCT.INT.ACCR,F.CAB.INTACCT.INT.ACCR,ERR.CAB.INT)

    LOOP
        REMOVE Y.ACCOUNT.ID FROM Y.ACCOUNT.ARR SETTING POS
    WHILE Y.ACCOUNT.ID:POS
*        IF INDEX(Y.ACCOUNT.ID,'12801',1) THEN DEBUG
        GOSUB READ.ACCOUNT
        IF START.DATE LT Y.AC.OPEN.DATE THEN
            START.DATE = Y.AC.OPEN.DATE
        END

        NO.DAYS.DR = ''
        NO.DAYS.CR = ''
        ACCT.AVG.BAL.DR = ''
        ACCT.AVG.BAL.CR = ''
        ACCT.AVG.BAL.CR = START.DATE
        ACCT.AVG.BAL.DR = END.DATE
        CALL CAB.ACCT.AVG.BAL.CALC(Y.ACCOUNT.ID,ACCT.AVG.BAL.CR,ACCT.AVG.BAL.DR,NO.DAYS.CR,NO.DAYS.DR)

        GOSUB READ.CCY.INT.TABLE
        GOSUB CALC.TOTAL.INT
    REPEAT

    GOSUB RAISE.ENTRY

    RETURN
*----------------------------------------------------------------------------------------------------------------------------------------
SUB.ACCOUNT.PROCESS:
* ------------------
    IF YFLAG NE '' THEN
        RETURN
    END
    R.AC.SUB.ACCOUNT = ''
    ERR.AC.SUB.ACCOUNT = ''
    CALL F.READ(FN.AC.SUB.ACCOUNT,Y.MASTER.ACCOUNT,R.AC.SUB.ACCOUNT,F.AC.SUB.ACCOUNT,ERR.AC.SUB.ACCOUNT)
    IF R.AC.SUB.ACCOUNT THEN
        Y.ACCOUNT.ARR = ''
        LOOP
            CUR.ACCT = ''
            CUR.POS = ''
            REMOVE CUR.ACCT FROM R.AC.SUB.ACCOUNT SETTING CUR.POS
        WHILE CUR.ACCT:CUR.POS
            IF CUR.ACCT THEN
                IF Y.ACCOUNT.ARR THEN
                    Y.ACCOUNT.ARR = Y.ACCOUNT.ARR:FM:CUR.ACCT
                END ELSE
                    Y.ACCOUNT.ARR = CUR.ACCT
                END
            END
        REPEAT
        Y.ACCOUNT.ARR = Y.MASTER.ACCOUNT:FM:Y.ACCOUNT.ARR
    END ELSE
        R.ACC = ''
        ERR.ACC = ''
        CALL F.READ(FN.ACCOUNT,Y.MASTER.ACCOUNT,R.ACC,F.ACCOUNT,ERR.ACC)
        IF ERR.ACC THEN
            MESSAGE.INFO<1> = 'CAB.B.INT.BUILD'
            MESSAGE.INFO<4> = 'Cannot find the ACCOUNT record '
            CALL FATAL.ERROR(MESSAGE.INFO)
        END
        Y.MASTER.FLAG = ''
        Y.MASTER.FLAG = R.ACC<AC.MASTER.ACCOUNT>
        IF NOT(Y.MASTER.FLAG) THEN
            Y.ACCOUNT.ARR = Y.MASTER.ACCOUNT
        END

    END
    RETURN
*-----------------------------------------------------------------------------------------------
READ.ACCOUNT:
*-----------
    R.ACCOUNT = ''
    ERR.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.ID,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    IF ERR.ACCOUNT THEN
        MESSAGE.INFO<1> = 'CAB.B.INT.BUILD'
        MESSAGE.INFO<4> = 'Cannot find the ACCOUNT record '
        CALL FATAL.ERROR(MESSAGE.INFO)
    END
    Y.CCY.ID = R.ACCOUNT<AC.CURRENCY>
    ST.CCY = Y.CCY.ID
    Y.POST.CCY = LCCY
    Y.CO.CODE = R.ACCOUNT<AC.CO.CODE>
    Y.CATEGORY = R.ACCOUNT<AC.CATEGORY>
    Y.ACCOUNT.OFFICER = R.ACCOUNT<AC.ACCOUNT.OFFICER>
    Y.CUST.NO = R.ACCOUNT<AC.CUSTOMER>
    Y.AC.OPEN.DATE = R.ACCOUNT<AC.OPENING.DATE>

    RETURN
*-----------------------------------------------------------------------------------------------
READ.CCY.INT.TABLE:
*------------------
    CALL F.READ(FN.CAB.CCY.INT.RATE,Y.CO.CODE,R.CAB.CCY.INT.RATE,F.CAB.CCY.INT.RATE,ERR.CAB.CCY.INT.RATE)
    IF R.CAB.CCY.INT.RATE THEN
        Y.PL.DEBIT.CATEG = R.CAB.CCY.INT.RATE<INT.BR.PL.DEBIT.CATEGORY>
        Y.PL.CREDIT.CATEG = R.CAB.CCY.INT.RATE<INT.BR.PL.CREDIT.CATEGORY>
        Y.WASH.ACCT = R.CAB.CCY.INT.RATE<INT.BR.WASH.ACCOUNT>
        Y.OFS.SRC.ID = R.CAB.CCY.INT.RATE<INT.BR.OFS.SOURCE.ID>
        Y.VERSION.ID = R.CAB.CCY.INT.RATE<INT.BR.VERSION.ID>
        Y.CCY.ARR = R.CAB.CCY.INT.RATE<INT.BR.CURRENCY>
        Y.HO.COMPANY = R.CAB.CCY.INT.RATE<INT.BR.HO.COMPANY>
        Y.POS1 = ''
        Y.POS2 = ''
        Y.POS3 = ''
        FIND Y.CCY.ID IN Y.CCY.ARR<1,1,1> SETTING Y.POS1,Y.POS2,Y.POS3 ELSE
            Y.POS1 = ''
            Y.POS2 = ''
            Y.POS3 = ''
        END
        LOCATE Y.CCY.ID IN Y.CCY.ARR<1,1> SETTING Y.POS1 ELSE
            Y.POS1 = ''
        END

        Y.INT.RATE.DR = R.CAB.CCY.INT.RATE<INT.BR.DR.INT.RATE,Y.POS1,1>
        Y.INT.DR.BASIS = R.CAB.CCY.INT.RATE<INT.BR.DR.INT.BASIS,Y.POS1,1>
        Y.INT.RATE.CR = R.CAB.CCY.INT.RATE<INT.BR.CR.INT.RATE,Y.POS1,1>
        Y.INT.CR.BASIS = R.CAB.CCY.INT.RATE<INT.BR.CR.INT.BASIS,Y.POS1,1>

        Y.EXCH.RATE = R.CAB.CCY.INT.RATE<INT.BR.MID.REVAL.RATE,Y.POS1,1>
        Y.CCY.MKT = R.CAB.CCY.INT.RATE<INT.BR.CURRENCY.MARKET>
    END

    RETURN
*---------------------------------------------------------------------------------------------------------------------
CALC.TOTAL.INT:
*--------------

    CALL F.READ(FN.INTEREST.BASIS,Y.INT.DR.BASIS,R.INT.BASIS.DR,F.INTEREST.BASIS,ERR.INT.BASIS.DR)
    IF R.INT.BASIS.DR THEN
        INT.BAS.DR = R.INT.BASIS.DR<IB.INT.BASIS>
        INT.BAS.DR = FIELD(INT.BAS.DR,"/",2)
    END
    CALL F.READ(FN.INTEREST.BASIS,Y.INT.CR.BASIS,R.INT.BASIS.CR,F.INTEREST.BASIS,ERR.INT.BASIS.CR)
    IF R.INT.BASIS.CR THEN
        INT.BAS.CR = R.INT.BASIS.CR<IB.INT.BASIS>
        INT.BAS.CR = FIELD(INT.BAS.CR,"/",2)
    END

    IF ACCT.AVG.BAL.DR NE '0' OR ACCT.AVG.BAL.DR NE '' OR NO.DAYS.DR NE '0' THEN
        CALC.DR = ACCT.AVG.BAL.DR*(Y.INT.RATE.DR/100)*(NO.DAYS.DR/INT.BAS.DR)
    END ELSE
        CALC.DR = '0'
    END
    IF ACCT.AVG.BAL.CR NE '0' OR ACCT.AVG.BAL.DR NE '' OR NO.DAYS.CR NE '0' THEN
        CALC.CR = ACCT.AVG.BAL.CR*(Y.INT.RATE.CR/100)*(NO.DAYS.CR/INT.BAS.CR)
    END ELSE
        CALC.CR = '0'
    END

    GOSUB CALC.DR.CR.AMT
    RETURN
*-----------------------------------------------------------------------------------------------------------------
CALC.DR.CR.AMT:
*--------------

    Y.AC.INT.AMT = CALC.DR + CALC.CR
*    CALL EB.ROUND.AMOUNT(Y.CCY.ID,Y.AC.INT.AMT,"","")

    CALL F.READ(FN.CURR,Y.CCY.ID,R.CURRENCY,F.CURR,CURR.ERR)
    IF R.CURRENCY THEN
        CCY.MK = R.CURRENCY<EB.CUR.CURRENCY.MARKET>
        LOCATE Y.CCY.MKT IN CCY.MK<1,1> SETTING CCY.POS THEN
            Y.EXCH.RATE = R.CURRENCY<EB.CUR.MID.REVAL.RATE,CCY.POS>
        END
    END ELSE
        Y.EXCH.RATE = ''
    END
    IF Y.AC.INT.AMT GT 0 THEN
        Y.TOT.CR.INT.AMT += Y.AC.INT.AMT
    END ELSE
        Y.TOT.DR.INT.AMT += Y.AC.INT.AMT
    END

    Y.TOTAL.INT += Y.TOT.CR.INT.AMT + Y.TOT.DR.INT.AMT
    CALL EB.ROUND.AMOUNT(Y.CCY.ID,Y.TOTAL.INT,"","")

    GOSUB UPDATE.INT.ACCR
    RETURN
*-------------------------------------------------------------------------------------------------------
UPDATE.INT.ACCR:
*--------------


    IF R.CAB.INTACCT.INT.ACCR EQ '' THEN
        R.CAB.INTACCT.INT.ACCR<INT.ACC.ACCOUNT.NO> = Y.ACCOUNT.ID
        R.CAB.INTACCT.INT.ACCR<INT.ACC.CURRENCY> = Y.CCY.ID
        R.CAB.INTACCT.INT.ACCR<INT.ACC.NO.OF.DAYS.DR> = NO.DAYS.DR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.AVG.BAL.DR> = ACCT.AVG.BAL.DR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.DR.INT.RATE> = Y.INT.RATE.DR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.INT.AMT.DR> = CALC.DR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.NO.OF.DAYS.CR> = NO.DAYS.CR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.AVG.BAL.CR> = ACCT.AVG.BAL.CR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.CR.INT.RATE> = Y.INT.RATE.CR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.INT.AMT.CR> = CALC.CR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.NET.INT.AMT> = Y.AC.INT.AMT
    END ELSE
        R.CAB.INTACCT.INT.ACCR<INT.ACC.ACCOUNT.NO>: = VM:Y.ACCOUNT.ID
        R.CAB.INTACCT.INT.ACCR<INT.ACC.CURRENCY> := VM:Y.CCY.ID
        R.CAB.INTACCT.INT.ACCR<INT.ACC.NO.OF.DAYS.DR> := VM:NO.DAYS.DR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.AVG.BAL.DR> := VM:ACCT.AVG.BAL.DR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.DR.INT.RATE> := VM:Y.INT.RATE.DR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.INT.AMT.DR> := VM:CALC.DR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.NO.OF.DAYS.CR> := VM:NO.DAYS.CR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.AVG.BAL.CR> := VM:ACCT.AVG.BAL.CR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.CR.INT.RATE> := VM:Y.INT.RATE.CR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.INT.AMT.CR> := VM:CALC.CR
        R.CAB.INTACCT.INT.ACCR<INT.ACC.NET.INT.AMT> := VM:Y.AC.INT.AMT
    END
*    R.CAB.INTACCT.INT.ACCR<INT.ACC.MID.REVAL.RATE> = Y.EXCH.RATE
    R.CAB.INTACCT.INT.ACCR<INT.ACC.PROCESS.DATE> = TODAY


*    CALL F.WRITE(FN.CAB.INTACCT.INT.ACCR,Y.ACCT.ACTIVITY.ID,R.CAB.INTACCT.INT.ACCR)

    Y.TOT.DR.INT.AMT = ''
    Y.TOT.CR.INT.AMT = ''

    RETURN
*-------------------------------------------------------------------------------------------------------------------------------------------------
RAISE.ENTRY:
*-----------

    IF YFLAG NE '' THEN
        RETURN
    END
    Y.REF = Y.MASTER.ACCOUNT:'-':START.DATE[1,6]

    FINAL.NET.AMT = Y.TOTAL.INT
*** AC_21 CR - 21-May-2013
    IF Y.CCY.ID NE LCCY THEN
*        LCY.AMOUNT = FINAL.NET.AMT * Y.EXCH.RATE
*        CALL EB.ROUND.AMOUNT(Y.POST.CCY,LCY.AMOUNT,"","")
*        Y.NET.AMT = LCY.AMOUNT
        CONV.FLAG = ''
        CCY.MKT = '1'
        AMOUNT.OUT = ''
        FGN.EQU.AMT = ''
        BASE.CCY = ''
        EXCHANGE.RATE = ''
        DIFFERENCE = ''
        RETURN.CODE = ''
        CALL EXCHRATE ("1",Y.CCY.ID,FINAL.NET.AMT,LCCY,FGN.EQU.AMT,BASE.CCY,EXCHANGE.RATE,DIFFERENCE,Y.LCY.AMT,RETURN.CODE)
        LCY.AMOUNT = FGN.EQU.AMT
        CALL EB.ROUND.AMOUNT(LCCY,FGN.EQU.AMT,"","")
        Y.NET.AMT = FGN.EQU.AMT
        Y.EXCH.RATE = EXCHANGE.RATE
*** AC_21 CR - 21-May-2013

    END ELSE
        LCY.AMOUNT = FINAL.NET.AMT
        CALL EB.ROUND.AMOUNT(Y.CCY.ID,LCY.AMOUNT,"","")
        Y.NET.AMT = LCY.AMOUNT
        FCY.AMOUNT = ''
    END
    PAY.NARR = Y.MASTER.ACCOUNT:'*':FINAL.NET.AMT:'*':LCY.AMOUNT

    OFS.SOURCE.ID = Y.OFS.SRC.ID
    OFS$SOURCE.ID = OFS.SOURCE.ID
    CALL F.READ(FN.OFS.SOURCE,OFS.SOURCE.ID,R.OFS.SOURCE,F.OFS.SOURCE,OFS.SOURCE.READ.ERR)
    IF R.OFS.SOURCE THEN
        OFS$SOURCE.REC = R.OFS.SOURCE
    END
*** AC_21 CR - 21-May-2013

    R.CAB.INTACCT.INT.ACCR<INT.ACC.MID.REVAL.RATE> = Y.EXCH.RATE
    CALL F.WRITE(FN.CAB.INTACCT.INT.ACCR,Y.ACCT.ACTIVITY.ID,R.CAB.INTACCT.INT.ACCR)

*** AC_21 CR - 21-May-2013

    IF Y.NET.AMT EQ 0 OR Y.NET.AMT EQ '' ELSE
        IF Y.CO.CODE NE Y.HO.COMPANY THEN
            IF Y.NET.AMT LT 0 THEN
                GOSUB DEBIT.FT
            END ELSE
                GOSUB CREDIT.FT
            END
        END
    END
    RETURN
*-------------------------------------------------------------------------------------
DEBIT.FT:
*--------
* This is the first FT transaction
* Debit wash a/c and credit PL of master a/c.
    ID.COMPANY = Y.HO.COMPANY
    Y.FT.OFS.STR = ''
    Y.OUT.RESP = ''
    Y.CO.CODE = R.ACCOUNT<AC.CO.CODE>
    CALL F.READ(FN.COMPANY,Y.CO.CODE,R.PL.COMPANY,F.COMPANY,Y.COM.ERR)
    PL.CREDIT.CATEGORY = "PL":Y.PL.CREDIT.CATEG:"\":R.PL.COMPANY<EB.COM.MNEMONIC>
    PL.DEBIT.CATEGORY = "PL":Y.PL.DEBIT.CATEG
    LCY.AMOUNT = LCY.AMOUNT * -1
    PAY.NARR.VAL = 'INTER':'-':Y.CO.CODE
    Y.FT.OFS.STR = Y.VERSION.ID:"/I/PROCESS,//,,"
    Y.FT.OFS.STR := "TRANSACTION.TYPE::=AC,"
    Y.FT.OFS.STR := "DEBIT.ACCT.NO::=":Y.WASH.ACCT:","
    Y.FT.OFS.STR := "DEBIT.CURRENCY::=":Y.POST.CCY:","
    Y.FT.OFS.STR := "DEBIT.AMOUNT::=":LCY.AMOUNT:","
    Y.FT.OFS.STR := "CREDIT.ACCT.NO::=":PL.CREDIT.CATEGORY:","
    Y.FT.OFS.STR := "ORDERING.BANK::=CAB,"
    Y.FT.OFS.STR := "LOCAL.REF:":PAY.POS:":=":PAY.NARR:","
    Y.FT.OFS.STR := "LOCAL.REF:":PAY.NARR.POS:":=":PAY.NARR.VAL:","
    Y.FT.OFS.STR := "PROFIT.CENTRE.DEPT::=1"
*    CALL OFS.BULK.MANAGER(Y.FT.OFS.STR,OFS.OUT.MSG,'')
    CALL OFS.POST.MESSAGE(Y.FT.OFS.STR,'',Y.OFS.SRC.ID,OPTIONS)
    Y.CONCAT.ID = ''
    R.CONCAT.REC = ''
    Y.B.ERR = ''
* End of first FT transaction
    GOSUB SECOND.FT.DR

    RETURN
*----------------------------------------------------------------------------------------
SECOND.FT.DR:
*-----------
* This is the second FT transaction
* Debit PL of HO and credit wash a/c
    Y.FT.OFS.STR = ''
    Y.OUT.RESP = ''
    Y.FT.OFS.STR = Y.VERSION.ID:"/I/PROCESS,//,,"
    Y.FT.OFS.STR := "TRANSACTION.TYPE::=AC,"
    Y.FT.OFS.STR := "DEBIT.ACCT.NO::=":PL.DEBIT.CATEGORY:","
    Y.FT.OFS.STR := "DEBIT.CURRENCY::=":Y.POST.CCY:","
    Y.FT.OFS.STR := "DEBIT.AMOUNT::=":LCY.AMOUNT:","
    Y.FT.OFS.STR := "CREDIT.ACCT.NO::=":Y.WASH.ACCT:","
    Y.FT.OFS.STR := "ORDERING.BANK::=CAB,"
    Y.FT.OFS.STR := "LOCAL.REF:":PAY.POS:":=":PAY.NARR:","
    Y.FT.OFS.STR := "PROFIT.CENTRE.DEPT::=1"
*    CALL OFS.BULK.MANAGER(Y.FT.OFS.STR,OFS.OUT.MSG,'')
    CALL OFS.POST.MESSAGE(Y.FT.OFS.STR,'',Y.OFS.SRC.ID,OPTIONS)
    Y.CONCAT.ID = ''
    R.CONCAT.REC = ''
    Y.B.ERR = ''
*End of second FT transaction
    RETURN

*----------------------------------------------------------------------------------------
CREDIT.FT:
*---------

* Credit balance - Debit PL of master a/c and credit PL of HO
*This is the First FT transaction of HO
* Debit PL of master a/c and credit wash a/c
    ID.COMPANY = Y.HO.COMPANY
    Y.FT.OFS.STR = ''
    Y.OUT.RESP = ''
    Y.CO.CODE = R.ACCOUNT<AC.CO.CODE>
    CALL F.READ(FN.COMPANY,Y.CO.CODE,R.PL.COMPANY,F.COMPANY,Y.COM.ERR)
    PL.DEBIT.CATEGORY = "PL":Y.PL.DEBIT.CATEG:"\":R.PL.COMPANY<EB.COM.MNEMONIC>
    PL.CREDIT.CATEGORY = "PL":Y.PL.CREDIT.CATEG
    PAY.NARR.VAL = 'INTER':'-':Y.CO.CODE

    Y.FT.OFS.STR = Y.VERSION.ID:"/I/PROCESS,//,,"
    Y.FT.OFS.STR := "TRANSACTION.TYPE::=AC,"
    Y.FT.OFS.STR := "DEBIT.ACCT.NO::=":PL.DEBIT.CATEGORY:","
    Y.FT.OFS.STR := "DEBIT.CURRENCY::=":Y.POST.CCY:","
    Y.FT.OFS.STR := "DEBIT.AMOUNT::=":LCY.AMOUNT:","
    Y.FT.OFS.STR := "CREDIT.ACCT.NO::=":Y.WASH.ACCT:","
    Y.FT.OFS.STR := "ORDERING.BANK::=CAB,"
    Y.FT.OFS.STR := "LOCAL.REF:":PAY.POS:":=":PAY.NARR:","
    Y.FT.OFS.STR := "LOCAL.REF:":PAY.NARR.POS:":=":PAY.NARR.VAL:","
    Y.FT.OFS.STR := "PROFIT.CENTRE.DEPT::=1"
*    CALL OFS.BULK.MANAGER(Y.FT.OFS.STR,OFS.OUT.MSG,'')
    CALL OFS.POST.MESSAGE(Y.FT.OFS.STR,'',Y.OFS.SRC.ID,OPTIONS)
    Y.CONCAT.ID = ''
    R.CONCAT.REC = ''
    Y.B.ERR = ''
*End of first ft transaction of HO
    GOSUB SECOND.FT.CR

    RETURN
*-----------------------------------------------------------------------------------
SECOND.FT.CR:
*------------
*This is the second FT transaction of HO
* Debit wash a/c and credit PL of HO
    Y.FT.OFS.STR = ''
    Y.OUY.RESP = ''
    Y.FT.OFS.STR = Y.VERSION.ID:"/I/PROCESS,//,,"
    Y.FT.OFS.STR := "TRANSACTION.TYPE::=AC,"
    Y.FT.OFS.STR := "DEBIT.ACCT.NO::=":Y.WASH.ACCT:","
    Y.FT.OFS.STR := "DEBIT.CURRENCY::=":Y.POST.CCY:","
    Y.FT.OFS.STR := "DEBIT.AMOUNT::=":LCY.AMOUNT:","
    Y.FT.OFS.STR := "CREDIT.ACCT.NO::=":PL.CREDIT.CATEGORY:","
    Y.FT.OFS.STR := "ORDERING.BANK::=CAB,"
    Y.FT.OFS.STR := "LOCAL.REF:":PAY.POS:":=":PAY.NARR:","
    Y.FT.OFS.STR := "PROFIT.CENTRE.DEPT::=1"
*    CALL OFS.BULK.MANAGER(Y.FT.OFS.STR,OFS.OUT.MSG,'')
    CALL OFS.POST.MESSAGE(Y.FT.OFS.STR,'',Y.OFS.SRC.ID,OPTIONS)
    Y.CONCAT.ID = ''
    R.CONCAT.REC = ''
    Y.B.ERR = ''

**End of second FT transaction of HO.
    YFLAG = "1"
    RETURN
*---------------------------------------------------------------------------------------------------
END
