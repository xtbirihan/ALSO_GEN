@EndUserText.label: 'ZZ1_SPED_Picking_Text'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.representativeKey: 'Code'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.dataClass: #CUSTOMIZING
@AbapCatalog.sqlViewName: 'ZZ1_F719F28880B8'
@Search.searchable
define view ZZ1_SPED_PICKING_TEXT_W
  as select from ZZ1_26360ED1B8FA
association [1..1] to ZZ1_SPED_PICKING_TEXT_V as _Code on $projection.Code = _Code.Code
association [0..1] to I_Language as _Language on $projection.Language = _Language.Language
{
  @Search.DefaultSearchElement: true
  key CODE     as Code,
  @Semantics.language: true
  @ObjectModel.foreignKey.association: '_Language'
  key LANGUAGE as Language,
  @Search.DefaultSearchElement: true
  @Semantics.text: true
  DESCRIPTION  as Description,
  @ObjectModel.association.type: [#TO_COMPOSITION_ROOT, #TO_COMPOSITION_PARENT]
  _Code,
  _Language
}
