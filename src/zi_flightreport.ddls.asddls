@AbapCatalog.sqlViewName: 'ZIFLGHTREP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight Report Test'
define view ZI_FlightReport
  as select from FDT_TEST_FLIGHT_CDS as Flight
{
  key Flight.carrid                                                                     as Carrid,
  key Flight.connid                                                                     as Connid,
      Flight.fldate                                                                     as Fldate,
      Flight.price                                                                      as Price,
      Flight.currency                                                                   as Currency,
      Flight.planetype                                                                  as PlaneType,
      Flight.paymentsum                                                                 as PaymentSum,

      Flight._scarr.CarrierName                                                         as CarrName,

      Flight._spfli.countryfr                                                           as CountryFr,
      Flight._spfli.cityfrom                                                            as CityFrom,
      Flight._spfli.airpfrom                                                            as AirpFrom,
      Flight._spfli.countryto                                                           as CountryTo,
      Flight._spfli.cityto                                                              as CityTo,
      Flight._spfli.airpto                                                              as AirpTo,
      Flight._spfli.fltime                                                              as FlTime,
      Flight._spfli.deptime                                                             as DepTime,
      Flight._spfli.arrtime                                                             as ArrTime,
      Flight._spfli.distance                                                            as Distance,
      Flight._spfli.distid                                                              as DistId,

      Flight._spfli._countryfr._Text[ Language = $session.system_language ].CountryName as CountryFr_Name,
      Flight._spfli._countryfr._Text[ Language = $session.system_language ].CountryName as CountryTo_Name,

      Flight._spfli._AirportFrom.AirportName                                            as AirpFrom_Name,
      Flight._spfli._AirportTo.AirportName                                              as AirpTo_Name
}
