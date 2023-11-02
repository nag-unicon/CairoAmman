    SUBROUTINE CAB.AI.EFWATER.COM(T1,T2,T3,T4,T5,T6,T7,T8,T9,CHG.AMT)
*-----------------------------------------------------------------------------
*Modification Details:
*=====================
* Date         Modified by                Description
* 16-10-2018    Nuha Tuhul                 Add versions OFC to 0.25 jd  Comm
*-----------------------------------------------------------------------------
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.TELLER

    DR.AMT = ''
    DR.AMT.1 = ''
    CHG.AMT = ''
    YR.CHARGE = ''



    IF PGM.VERSION  MATCHES   ",LCY.POSTPAID.CASH":@VM:",LCY.PREPAID.CASH"  THEN
        YR.CHARGE = 1
        CHG.AMT = YR.CHARGE
        RETURN
    END ELSE
        IF PGM.VERSION  MATCHES   ",LCY.POSTPAID.CASH.OFC":@VM:",LCY.PREPAID.CASH.OFC"  THEN
            YR.CHARGE = 0.25
            CHG.AMT = YR.CHARGE
            RETURN
        END ELSE
            RETURN
        END
    END

    RETURN
