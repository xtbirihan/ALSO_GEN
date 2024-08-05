@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_1938D30F5542'

extend view NSDM_E_MARC with ZZ1_WBUNC7DXEKBWWUUYEJQCUO2Q5M
    association [0..1] to ZZ1_OPTI_V as _ZZ1_OPTI_PLT
  on  $projection.ZZ1_OPTI_PLT = _ZZ1_OPTI_PLT.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_OPTI_PLT'
  _ActiveExtension.ZZ1_OPTI_PLT as ZZ1_OPTI_PLT,
  _ZZ1_OPTI_PLT
}
