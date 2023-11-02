    SUBROUTINE CAB.AA.VIRTUAL.BAL(ACC.ID,BASE.BALANCE,BAL.AMOUNT)
*******************************************************************
*** This routine will return the balance amount for
*** balance type for account
***
***
*******************************************************************

    $INCLUDE T24.BP I_F.AC.BALANCE.TYPE
    $INCLUDE T24.BP I_F.ACCT.ACTIVITY
    $INCLUDE T24.BP I_F.AA.INTEREST
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE

    GOSUB GET.ENQUIRY.BALANCE
    GOSUB GET.BALANCE.AMOUNT

    RETURN

********************
GET.ENQUIRY.BALANCE:
********************
    BAL.AMOUNT = '0'
    BASE.BALANCE = 'ETOTALDUE'
    GOSUB CHECK.AC.BALANCE.TYPE
    BAL.AMOUNT = BALANCE.TYPE.AMT

    RETURN
*******************
GET.BALANCE.AMOUNT:
*******************

    BAL.DATE = TODAY
    BAL.DETS = ''
    CALL AC.GET.PERIOD.BALANCES(ACC.ID, BASE.BALANCE, '', BAL.DATE,'', '', BAL.DETS, ERR.PROCESS)
    BALANCE.TYPE.AMT += BAL.DETS<IC.ACT.BALANCE>

    RETURN

**********************
CHECK.AC.BALANCE.TYPE:
**********************
    R.BALANCE.TYPE = ''
    BALANCE.TYPE.AMT = ''
    CALL CACHE.READ("F.AC.BALANCE.TYPE", BASE.BALANCE, R.BALANCE.TYPE, VAL.ERR)

    BASE.BALANCES = R.BALANCE.TYPE<AC.BT.VIRTUAL.BAL>
    BASE.BAL.COUNT = DCOUNT(BASE.BALANCES, VM)
    FOR BAL.COUNTER = 1 TO BASE.BAL.COUNT
        BASE.BALANCE = BASE.BALANCES<1, BAL.COUNTER>
        GOSUB GET.BALANCE.AMOUNT
    NEXT BAL.COUNTER

    RETURN
