@AbapCatalog.sqlViewAppendName: 'ZESPFLICDS'
@EndUserText.label: 'Spfli CDS Extend'
extend view Fdt_Test_Pfli_Cds with ZE_Spfli_Cds
  association [1..1] to SADL_V_ExpVoc_Airport as _AirportFrom on $projection.AirportFrom = _AirportFrom.Airport
  association [1..1] to SADL_V_ExpVoc_Airport as _AirportTo   on $projection.AirportTo = _AirportTo.Airport
{
  spfli.airpfrom as AirportFrom,
  spfli.airpto   as AirportTo,

  _AirportFrom,
  _AirportTo
}
