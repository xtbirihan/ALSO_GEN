@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_6D7AEC151EB4'

extend view P_PRDSCM_MATLOC with ZZ1_ELW45N5CTP4BK67QXQX3PASZKI
    association [0..1] to ZZ1_OPTI_V as _ZZ1_OPTI_PLT
  on  $projection.ZZ1_OPTI_PLT = _ZZ1_OPTI_PLT.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_OPTI_PLT'
  _ActiveExtension.ZZ1_OPTI_PLT as ZZ1_OPTI_PLT,
  _ZZ1_OPTI_PLT
}
