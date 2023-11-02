*-----------------------------------------------------------------------------
* <Rating>-60</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE CAB.AUT.DEFAULT.ENROL.ARC
*---------------------------------------------------------------
*Company   Name    : Cairo Amman Bank Palestine
*Developed By      : Temenos
*Program   Name    : CAB.AUT.DEFAULT.ENROL.ARC
*---------------------------------------------------------------
*Description       :
*Linked With       :
*In  Parameter     : N/A
*Out Parameter     : N/A
*ODR number used   :
*---------------------------------------------------------------
*Modification Details:
*=====================
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
    $INSERT I_GTS.COMMON
    $INSERT I_F.ACCOUNT

    GOSUB INIT
    GOSUB PROCESS

    RETURN

****
INIT:
****

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    CALL OPF(FN.ACC,F.ACC)

    RETURN

********
PROCESS:
********

    CALL F.READ(FN.ACC,ID.NEW,R.ACC,F.ACC,E.ACC)
    IF NOT(E.ACC) THEN

        R.NEW(AC.CUSTOMER) = R.ACC<AC.CUSTOMER>
        R.NEW(AC.CATEGORY) = R.ACC<AC.CATEGORY>
        R.NEW(AC.CURRENCY) = R.ACC<AC.CURRENCY>

    END
    RETURN
END
