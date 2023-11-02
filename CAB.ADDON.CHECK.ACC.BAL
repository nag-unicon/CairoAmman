    SUBROUTINE CAB.ADDON.CHECK.ACC.BAL

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT CAB.BP I_F.CAB.EXTRA.TAX.DETS
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT CAB.BP I_F.CAB.EXTRA.TAX.FIRST.RUN
    $INSERT CAB.BP I_F.CAB.EXTRA.TAX.ACC.DETS

    GOSUB INITIALISE
    IF Y.SEL.LIST NE '' THEN
        GOSUB PROCESS
    END

    RETURN

INITIALISE:

    Y.POST.DATE = '' ; Y.END.DATE = ''
    Y.CURRENT.YEAR = TODAY[1,4]
    IF Y.CURRENT.YEAR = 2021 THEN
        Y.POST.DATE = 20210531
        Y.END.DATE = 20211231
    END ELSE
        Y.POST.DATE = TODAY[1,4]:'0101'
        Y.END.DATE = Y.POST.DATE[1,4]:'1231'
    END

    F.TAX.FT.DETS = ''
    FN.TAX.FT.DETS = 'F.CAB.EXTRA.TAX.DETS'
    CALL OPF(FN.TAX.FT.DETS,F.TAX.FT.DETS)

    F.FIRST.RUN = ''
    FN.FIRST.RUN = 'F.CAB.EXTRA.TAX.FIRST.RUN'
    CALL OPF(FN.FIRST.RUN,F.FIRST.RUN)

    F.TAX.ACC.DETS = ''
    FN.TAX.ACC.DETS = 'F.CAB.EXTRA.TAX.ACC.DETS'
    CALL OPF(FN.TAX.ACC.DETS,F.TAX.ACC.DETS)

    F.ACC = ''
    FN.ACC = 'F.ACCOUNT'
    CALL OPF(FN.ACC,F.ACC)

    Y.SEL.CMD = '' ; Y.SEL.LIST = ''
    Y.SELECT.COMMAND = '' ; Y.SELECT.LIST = ''
    Y.SELECT.COMMAND = 'SELECT ':FN.FIRST.RUN
    CALL EB.READLIST(Y.SELECT.COMMAND,Y.SELECT.LIST,'','','')
    IF Y.SELECT.LIST EQ '' THEN
        Y.SEL.CMD = 'SELECT ':FN.TAX.FT.DETS:' LIKE ...-':Y.END.DATE:'...'
        CALL EB.READLIST(Y.SEL.CMD,Y.SEL.LIST,'','','')
    END

    RETURN

PROCESS:


    Y.ACC.IDS = '' ; Y.TAX.AMTS = '' ; Y.AVAIL.BALS = '' ; Y.ONLINE.BALANCE = '' ; Y.LOCKED.AMT = ''

    LOOP
    UNTIL Y.SEL.LIST<1> = ''
        Y.KEY = '' ; Y.KEY = Y.SEL.LIST<1>
        R.TAX.DETS = '' ; Y.RD.ERR = ''
        CALL F.READ(FN.TAX.FT.DETS,Y.KEY,R.TAX.DETS,F.TAX.FT.DETS,Y.RD.ERR)
        IF R.TAX.DETS NE '' THEN
            FOR Y.CNT1 = 1 TO DCOUNT(R.TAX.DETS<CABXTRATAX.TAX.FOR.ENTRY>,@VM)
                Y.AMT.TO.TAX = '' ; Y.TAX.PERC = '' ; Y.ACC.ID = '' ; Y.TAX.AMT = ''
                Y.AMT.TO.TAX = R.TAX.DETS<CABXTRATAX.TAX.FOR.LCY.AMT,Y.CNT1>
                Y.TAX.PERC = R.TAX.DETS<CABXTRATAX.EXTRA.TAX.PERC,Y.CNT1>
                Y.ACC.ID = R.TAX.DETS<CABXTRATAX.ACCOUNT.ID,Y.CNT1>
                IF NUM(Y.ACC.ID) THEN
                    Y.TAX.AMT = Y.AMT.TO.TAX*(Y.TAX.PERC/100)
                    Y.TAX.AMT = FMT(Y.TAX.AMT,'R2')
                    Y.ACC.BAL = ''
                    IF Y.TAX.AMT = 0 THEN Y.TAX.AMT = 0.01
                    R.ACC = '' ; Y.RD.ERR = ''
                    CALL F.READ(FN.ACC,Y.ACC.ID,R.ACC,F.ACC,Y.RD.ERR)
                    IF R.ACC NE '' THEN
                        Y.WRK.BAL = '' ; Y.AVAIL.BAL = ''
                        CALL CAB.ICBS.AVAIL.BALANCE(Y.ACC.ID,R.ACC,Y.WRK.BAL,Y.AVAIL.BAL)
                        IF Y.AVAIL.BAL = '' THEN  Y.AVAIL.BAL = 0
                        IF R.ACC<AC.CURRENCY> NE LCCY THEN
                            Y.OUT.AMT = '' ; Y.LCY.AMT = '' ; Y.RET.CODE = '' ; Y.RATE = ''
                            CALL EXCHRATE(1,LCCY,Y.TAX.AMT,R.ACC<AC.CURRENCY>,Y.OUT.AMT,"",Y.RATE,"",Y.LCY.AMT,Y.RET.CODE)
                            Y.TAX.AMT = Y.OUT.AMT
                        END
                        U.LOCK.AMT = 0 ; Y.DATE = TODAY
                        IF R.ACC<AC.FROM.DATE> NE '' THEN
                            Y.CTR = '' ; Y.CTR = DCOUNT(R.ACC<AC.FROM.DATE>,@VM)
                            FOR U.I = 1 TO Y.CTR
                                IF Y.DATE GE R.ACC<AC.FROM.DATE,U.I> THEN
                                    U.LOCK.AMT = R.ACC<AC.LOCKED.AMOUNT,U.I>
                                END
                            NEXT U.I
                        END
                        IF U.LOCK.AMT = '' THEN U.LOCK.AMT = 0
                        Y.ACC.POS = ''
                        LOCATE Y.ACC.ID IN Y.ACC.IDS<1> SETTING Y.ACC.POS ELSE Y.ACC.POS = ''
                        IF Y.ACC.POS = '' THEN
                            Y.ACC.IDS<-1> = Y.ACC.ID
                            Y.TAX.AMTS<-1> = Y.TAX.AMT
                            Y.AVAIL.BALS<-1> = Y.AVAIL.BAL
                            Y.CLEARED.BAL = '' ; Y.CLEARED.BAL = R.ACC<AC.ONLINE.CLEARED.BAL>
                            IF Y.CLEARED.BAL = '' THEN Y.CLEARED.BAL = 0
                            Y.ONLINE.BALANCE<-1> = Y.CLEARED.BAL
                            Y.LOCKED.AMT<-1> = U.LOCK.AMT
                        END ELSE
                            Y.TAX.AMTS<Y.ACC.POS> += Y.TAX.AMT
                        END
                    END
                END
            NEXT Y.CNT1
        END
        DELETE F.TAX.FT.DETS,Y.KEY
        DEL Y.SEL.LIST<1>
    REPEAT


    FOR Y.ACC.CNT = 1 TO DCOUNT(Y.ACC.IDS,@FM)
        IF Y.AVAIL.BALS<Y.ACC.CNT> GE Y.TAX.AMTS<Y.ACC.CNT> ELSE
            Y.ID.ACC = '' ; R.TAX.ACC.DETS = ''
            Y.ID.ACC = Y.ACC.IDS<Y.ACC.CNT>
            R.TAX.ACC.DETS<CABXTRAACC.EXTRA.TAX.AMT> = Y.TAX.AMTS<Y.ACC.CNT>
            R.TAX.ACC.DETS<CABXTRAACC.AVAILABLE.BALANCE> = Y.AVAIL.BALS<Y.ACC.CNT>
            R.TAX.ACC.DETS<CABXTRAACC.ONLINE.CLEARED.BAL> = Y.ONLINE.BALANCE<Y.ACC.CNT>
            R.TAX.ACC.DETS<CABXTRAACC.LOCKED.AMOUNT> = Y.LOCKED.AMT<Y.ACC.CNT>
            IF R.TAX.ACC.DETS NE '' THEN
                CALL F.WRITE(FN.TAX.ACC.DETS,Y.ID.ACC,R.TAX.ACC.DETS)
            END
        END
    NEXT Y.ACC.CNT

    R.LAST.RUN = ''
    R.LAST.RUN<CABXTRAFIRST.FIRST.RUN.DATE> = TODAY
    CALL F.WRITE(FN.FIRST.RUN,TODAY,R.LAST.RUN)

    RETURN

END
