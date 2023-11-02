*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE CAB.B.CHQ.ISSUE.RES(CHQ.ISSUE.ID)
*-------------------------------------------------------------------------------------------------------------------
* Company   Name    : CABK Bank
* Developed By      : Temenos Application Management
* Reference         : ODR-2011-10-0063
*-------------------------------------------------------------------------------------------------------------------
* Program Name      : CAB.B.CHQ.ISSUE.RES
* Author            : Abdul Rahman. J
* Date              : 23 JAN 2012
*-------------------------------------------------------------------------------------------------------------------
* Description       :
* Type              : BATCH Routine
* Linked with       : Attached to the BATCH record
* In  Parameter     : None
* Out Parameter     : None
*-------------------------------------------------------------------------------------------------------------------
* Modification History
* Reference     Date
*
*-------------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CHEQUE.ISSUE
    $INCLUDE CAB.BP I_CAB.CHQ.RESERVED.COMMON
    $INCLUDE CAB.BP I_F.CHEQUE.ISSUE.RESERVED


*    TEM.FILE = SEL.FILE.NAME

    CALL F.READ(FN.CHEQUE.ISSUE,CHQ.ISSUE.ID,R.CHQ.ISS,F.CHEQUE.ISSUE,ERR.CHQ)
    R.CHK.RES.REC<PY.DET.L.CUST.ID> =     R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,CUST.ID.POS>
    R.CHK.RES.REC<PY.DET.L.ACCT.NO> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,ACCT.NO.POS>
    R.CHK.RES.REC<PY.DET.L.CHQ.BOK.TITLE> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,CHQ.BOK.TITLE.POS>
    R.CHK.RES.REC<PY.DET.L.CHQ.TYPE> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,CHQ.TYPE.POS>
    R.CHK.RES.REC<PY.DET.CURRENCY> = R.CHQ.ISS<CHEQUE.IS.CURRENCY>
    R.CHK.RES.REC<PY.DET.CO.CODE> = R.CHQ.ISS<CHEQUE.IS.CO.CODE>
    R.CHK.RES.REC<PY.DET.L.NOOFBOOKS> =     R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,NOOFBOOKS.POS>
    R.CHK.RES.REC<PY.DET.L.NO.OF.LEAVES> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,NO.OF.LEAVES.POS>
    R.CHK.RES.REC<PY.DET.L.CHQ.START.NO> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,CHQ.START.NO.POS>
    R.CHK.RES.REC<PY.DET.L.CHQ.NO.END> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,CHQ.NO.END.POS>
    R.CHK.RES.REC<PY.DET.L.RECEIVINGBRN> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,RECEIVINGBRN.POS>
    R.CHK.RES.REC<PY.DET.L.AUTH.PER> =   R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,AUTH.PER.POS>
    R.CHK.RES.REC<PY.DET.L.MOBILE.NO> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,MOBILE.NO.POS>
    R.CHK.RES.REC<PY.DET.L.PAPER.TYPE> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,PAPER.TYPE.POS>
    R.CHK.RES.REC<PY.DET.L.PRINTING.TYPE> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,PRINTING.TYPE.POS>
    R.CHK.RES.REC<PY.DET.L.CUST.NAME.TYP> = R.CHQ.ISS<CHEQUE.IS.LOCAL.REF><1,CUST.NAME.TYP.POS>
    R.CHK.RES.REC<PY.DET.CHEQUE.STATUS> = R.CHQ.ISS<CHEQUE.IS.CHEQUE.STATUS>

    CALL F.WRITE(FN.CHEQUE.ISSUE.RESERVED,CHQ.ISSUE.ID,R.CHK.RES.REC)
    RETURN
END
