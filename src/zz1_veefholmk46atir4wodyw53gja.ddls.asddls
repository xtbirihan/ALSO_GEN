@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_1E7697CDDFD0'

extend view E_PRODUCTPLANT with ZZ1_VEEFHOLMK46ATIR4WODYW53GJA
    association [0..1] to ZZ1_OPTI_V as _ZZ1_OPTI_PLT
  on  $projection.ZZ1_OPTI_PLT = _ZZ1_OPTI_PLT.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_OPTI_PLT'
  Persistence.ZZ1_OPTI_PLT as ZZ1_OPTI_PLT,
  _ZZ1_OPTI_PLT
}
