*-----------------------------------------------------------------------------
* <Rating>-13</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE CAB.ACCT.CON.SETTL.BAL
*-----------------------------------------------------------------------------
*<doc>
* Common Parameter Table For Mobile Banking Interface
* @author - Gokul J
* @stereotype Application
* @package TODO define the product group and product, e.g. infra.eb
* </doc>
*-----------------------------------------------------------------------------
* TODO - You MUST write a .FIELDS routine for the field definitions
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------
* ----------------------------------------------------------------------------
* <region name= INCLUDE T24.BPs>
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_Table
* </region>
*-----------------------------------------------------------------------------
    Table.name = 'CAB.ACCT.CON.SETTL.BAL'         ;* Full application name including product prefix
    Table.title = 'CAB.ACCT.CON.SETTL.BAL'        ;* Screen title
    Table.stereotype = 'L'    ;* H, U, L, W or T
    Table.product = 'EB'      ;* Must be on EB.PRODUCT
    Table.subProduct = ''     ;* Must be on EB.SUB.PRODUCT
    Table.classification = 'INT'        ;* As per FILE.CONTROL
    Table.systemClearFile = 'Y'         ;* As per FILE.CONTROL
    Table.relatedFiles = ''   ;* As per FILE.CONTROL
    Table.isPostClosingFile = ''        ;* As per FILE.CONTROL
    Table.equatePrefix = 'CAB.CONSA'     ;* Use to create I_F.CAB.ACCT.CON.SETTL.BAL
*-----------------------------------------------------------------------------
    Table.idPrefix = ''       ;* Used by EB.FORMAT.ID if set
    Table.blockedFunctions = ''         ;* Space delimeted list of blocked functions
    Table.trigger = ''        ;* Trigger field used for OPERATION style fields
*-----------------------------------------------------------------------------

    RETURN
END
