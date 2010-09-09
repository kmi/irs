;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology wsmo-use-case-purchase)


(def-class pip3A4Purchase()
((has-buyer :type buyer
        :max-cardinality 1)
 (has-globaldocumentfunctioncode :type globalDocumentFunctionCode
                                 :max-cardinality 1)
 (has-purchaseorder :type purchaseOrder
                    :max-cardinality 1)
 (has-thisdocumentgenerationdatetime :type thisDocumentGenerationDateTime
                                     :max-cardinality 1)
 (has-thisdocumentidentifier :type thisDocumentIdentifier
                             :max-cardinality 1)
 (has-seller :type seller
             :max-cardinality 1)))

(def-class buyer()
 (has-partnerroledescription :type partnerRoleDescription
                             :max-cardinality 1)))

(def-class seller()
 (has-partnerroledescription :type partnerRoleDescription
             :max-cardinality 1)))

(def-class partnerRoleDescription
((has-contactinformation :type contactInformation
             :max-cardinality 1)
 (has-globalpartnerroleclassificationcode :type globalPartnerRoleClassificationCode
             :max-cardinality 1)
 (has-partnerdescription :type partnerDescription
             :max-cardinality 1)))

(def-class contactInformation()
 (has-contactname :type contactName
             :max-cardinality 1)
 (has-emailaddress :type emailAddress
             :max-cardinality 1)
 (has-facsimilenumber :type facsimileNumber
             :max-cardinality 1)
 (has-telephonenumber :type telephoneNumber
             :max-cardinality 1)
 (has-physicallocation :type physicalLocation
             :max-cardinality 1)))

(def-class contactName(string))

(def-class freeFormText (string))

(def-class emailAddress (string))

(def-class facsimileNumber
((has-communicationsnumber :type communicationsNumber
             :max-cardinality 1)))

(def-class communicationsNumber(string))

(def-class telephoneNumber
((has-communicationsnumber :type communicationsNumber
             :max-cardinality 1)))

(def-class globalPartnerRoleClassificationCode (string))

(def-class partnerDescription()
((has-businessdescription :type businessDescription
             :max-cardinality 1)
 (has-globalpartnerclassificationcode :type globalPartnerClassificationCode
             :max-cardinality 1)
 (has-contactinformation :type contactInformation
             :max-cardinality 1)
 (has-physicallocation :type physicalLocation
             :max-cardinality 1)))

(def-class businessDescription()
((has-globalbusinessidentifier :type globalBusinessIdentifier
             :max-cardinality 1)
 (has-globalsupplychaincode :type globalSupplyChainCode
             :max-cardinality 1)
 (has-businessname :type businessName
             :max-cardinality 1)
 (has-partnerbusinessidentification :type partnerBusinessIdentification
             :max-cardinality 1)
 (has-nationalbusinesstaxidentifier :type nationalBusinessTaxIdentifier
             :max-cardinality 1)))

(def-class globalBusinessIdentifier (integer))

(def-class globalSupplyChainCode(string)

(def-class globalPartnerClassificationCode(string)

(def-class globalDocumentFunctionCode(string)

(def-class purchaseOrder()
((has-accountdescription :type accountDescription
             :max-cardinality 1)
 (has-freeformtext :type freeFormText
             :max-cardinality 1)
 (has-contractinformation :type contractInformation
             :max-cardinality 1)
 (has-documentreference :type documentReference
             :max-cardinality 1)
 (has-financingterms :type financingTerms
             :max-cardinality 1)
 (has-generalservicesadministrationnumber :type generalServicesAdministrationNumber
             :max-cardinality 1)
 (has-globalgovernmentpriorityratingcode :type globalGovernmentPriorityRatingCode
             :max-cardinality 1)
 (has-globalpurchaseorderfillprioritycode :type globalPurchaseOrderFillPriorityCode
             :max-cardinality 1)
 (has-globalpurchaseordertypecode :type globalPurchaseOrderTypeCode
             :max-cardinality 1)
 (has-governmentcontractidentifier :type governmentContractIdentifier
             :max-cardinality 1)
 (has-installat :type installAt
             :max-cardinality 1)
 (has-isdropship :type isDropShip
             :max-cardinality 1)
 (has-ordershippinginformation :type orderShippingInformation
             :max-cardinality 1)
 (has-productlineitem :type productLineItem
             :max-cardinality 1)
 (has-proprietaryinformation :type proprietaryInformation
             :max-cardinality 1)
 (has-requestedevent :type requestedEvent
             :max-cardinality 1)
 (has-requestedshipfrom :type requestedShipFrom
             :max-cardinality 1)
 (has-secondarybuyer :type secondaryBuyer
             :max-cardinality 1)
 (has-shipto :type shipTo
             :max-cardinality 1)
 (has-taxexemptstatus :type taxExemptStatus
             :max-cardinality 1)
 (has-totalamount :type totalAmount
             :max-cardinality 1)))

(def-class accountDescription()
             :max-cardinality 1)
 (has-accountname :type accountName
             :max-cardinality 1)
 (has-accountnumber :type accountNumber
             :max-cardinality 1)
 (has-billto :type billTo
             :max-cardinality 1)
 (has-creditcard :type creditCard
             :max-cardinality 1)
 (has-financedby :type financedBy
             :max-cardinality 1)
 (has-globalaccountclassificationcode :type globalAccountClassificationCode
             :max-cardinality 1)
 (has-prepaymentchecknumber :type prePaymentCheckNumber
             :max-cardinality 1)
 (has-wiretransferidentifier :type wireTransferIdentifier
             :max-cardinality 1)))

(def-class accountName()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class accountNumber(string))

(def-class billTo()
((has-partnerdescription :type partnerDescription
             :max-cardinality 1)))

(def-class businessName()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class partnerBusinessIdentification()
((has-proprietarybusinessidentifier :type proprietaryBusinessIdentifier
             :max-cardinality 1)
 (has-proprietarydomainidentifier :type proprietaryDomainIdentifier
             :max-cardinality 1)
 (has-proprietaryidentifierauthority :type proprietaryIdentifierAuthority
             :max-cardinality 1)))

(def-class proprietaryBusinessIdentifier(xsd:string))

(def-class proprietaryDomainIdentifier(xsd:string))

(def-class proprietaryIdentifierAuthority(string)

(def-class physicalLocation()
((has-partnerlocationidentification :type partnerLocationIdentification
             :max-cardinality 1)
 (has-physicaladdress :type physicalAddress
             :max-cardinality 1)))

(def-class partnerLocationIdentification
((has-proprietarydomainidentifier :type proprietaryDomainIdentifier
             :max-cardinality 1)
 (has-proprietaryidentifierauthority :type proprietaryIdentifierAuthority
             :max-cardinality 1)
 (has-proprietarylocationidentifier :type proprietaryLocationIdentifier
             :max-cardinality 1)))

(def-class proprietaryLocationIdentifier(string))

(def-class physicalAddress()
((has-addressline :type addressLine
             :max-cardinality 1)
 (has-cityname :type cityName
             :max-cardinality 1)
 (has-globalcountrycode :type globalCountryCode
             :max-cardinality 1)
 (has-nationalpostalcode :type nationalPostalCode
             :max-cardinality 1)
 (has-postofficeboxidentifier :type postOfficeBoxIdentifier
             :max-cardinality 1)
 (has-regionname :type regionName
             :max-cardinality 1)
 (has-globallocationidentifier :type globalLocationIdentifier
             :max-cardinality 1)
 (has-partnerlocationidentification :type partnerLocationIdentification
             :max-cardinality 1)))

(def-class addressLine()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class cityName()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class globalCountryCode(string))

(def-class nationalPostalCode(string))

(def-class postOfficeBoxIdentifier()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class regionName()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class creditCard()
((has-cardholdername :type cardHolderName
             :max-cardinality 1)
 (has-creditcardidentifier :type creditCardIdentifier
             :max-cardinality 1)
 (has-expirydate :type expiryDate
             :max-cardinality 1)
 (has-globalcreditcardclassificationcode :type globalCreditCardClassificationCode
             :max-cardinality 1)
 (has-proprietarycididentifier :type proprietaryCIDIdentifier
             :max-cardinality 1)))

(def-class cardHolderName()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class creditCardIdentifier()
((has-proprietaryreferenceidentifier :type proprietaryReferenceIdentifier
             :max-cardinality 1)))

(def-class proprietaryReferenceIdentifier(string))

;; monthOfYear, year from date-time
(def-class expiryDate()
((has-expMonth :type monthOfYear
             :max-cardinality 1)
 (has-expYear :type year
             :max-cardinality 1)))

(def-class globalCreditCardClassificationCode(string))

(def-class proprietaryCIDIdentifier(string))

(def-class financedBy()
((has-partnerdescription :type partnerDescription
             :max-cardinality 1)))

(def-class globalAccountClassificationCode(string))

(def-class prePaymentCheckNumber()
((has-checknumber :type checkNumber
             :max-cardinality 1)))

(def-class checkNumber(string))

(def-class wireTransferIdentifier(string))

(def-class contractInformation()
((has-contractidentifier :type contractIdentifier
             :max-cardinality 1)
 (has-primarycontractwith :type primaryContractWith
             :max-cardinality 1)
 (has-secondarycontractwith :type secondaryContractWith
             :max-cardinality 1)))

(def-class contractIdentifier()
((has-proprietarydocumentidentifier :type proprietaryDocumentIdentifier
             :max-cardinality 1)))

(def-class proprietaryDocumentIdentifier(string))

(def-class primaryContractWith()
((has-partnerdescription :type partnerDescription
             :max-cardinality 1)))

(def-class secondaryContractWith()
((has-partnerdescription :type partnerDescription
             :max-cardinality 1)))

;; dateAndTime from date-time
(def-class documentReference()
((has-datetimestamp :type dateAndTime
             :max-cardinality 1)
 (has-globaldocumentreferencetypecode :type globalDocumentReferenceTypeCode
             :max-cardinality 1)
 (has-globalpartnerroleclassificationcode :type globalPartnerRoleClassificationCode
             :max-cardinality 1)
 (has-linenumber :type lineNumber
             :max-cardinality 1)
 (has-proprietarydocumentidentifier :type proprietaryDocumentIdentifier
             :max-cardinality 1)
 (has-revisionnumber :type revisionNumber
             :max-cardinality 1)))

(def-class globalDocumentReferenceTypeCode(string))

(def-class lineNumber(string))

(def-class revisionNumber(string))
 
(def-class financingTerms()
((has-globalfinancetermscode :type globalFinanceTermsCode
             :max-cardinality 1)
 (has-paymentterms :type paymentTerms
             :max-cardinality 1)))

(def-class globalFinanceTermsCode(string))

(def-class paymentTerms()
((has-discounts :type discounts
             :max-cardinality 1)
 (has-globalpaymentconditioncode :type globalPaymentConditionCode
             :max-cardinality 1)
 (has-nettermsday :type netTermsDay
             :max-cardinality 1)
 (has-nettermsdays :type netTermsDays
             :max-cardinality 1)
 (has-percentdue :type percentDue
             :max-cardinality 1)))

(def-class discounts()
((has-discountday :type discountDay
             :max-cardinality 1)
 (has-discountdays :type discountDays
             :max-cardinality 1)
 (has-discountpercent :type discountPercent
             :max-cardinality 1)))

;; dayOfMonth from date-time
(def-class discountDay()
((has-dayofmonth :type dayOfMonth
             :max-cardinality 1)))

(def-class discountDays()
((has-countableamount :type countableAmount
             :max-cardinality 1)))

(def-class countableAmount (integer))

(def-class discountPercent()
((has-percentamount :type percentAmount
             :max-cardinality 1)))

(def-class percentAmount(float))

(def-class globalPaymentConditionCode(string))

;; dayOfMonth from date-time
(def-class netTermsDay()
((has-dayofmonth :type dayOfMonth
             :max-cardinality 1)))

(def-class netTermsDays()
((has-countableamount :type countableAmount
             :max-cardinality 1)))

(def-class percentDue()
((has-percentamount :type percentAmount
             :max-cardinality 1)))

(def-class generalServicesAdministrationNumber()
((has-proprietarydocumentidentifier :type proprietaryDocumentIdentifier
             :max-cardinality 1)))

(def-class globalGovernmentPriorityRatingCode(string))

(def-class globalPurchaseOrderFillPriorityCode(string))

(def-class globalPurchaseOrderTypeCode(string))

(def-class governmentContractIdentifier()
((has-proprietarydocumentidentifier :type proprietaryDocumentIdentifier
             :max-cardinality 1)))

(def-class installAt()
((has-partnerdescription :type partnerDescription
             :max-cardinality 1)))

(def-class isDropShip()
((has-affirmationindicator :type affirmationIndicator
             :max-cardinality 1)))

(def-class affirmationIndicator(string))

(def-class orderShippingInformation()
((has-carrierinformation :type carrierInformation
             :max-cardinality 1)
 (has-globalfreeonboardcode :type globalFreeOnBoardCode
             :max-cardinality 1)
 (has-globalshipmenttermscode :type globalShipmentTermsCode
             :max-cardinality 1)
 (has-globalshippingservicelevelcode :type globalShippingServiceLevelCode
             :max-cardinality 1)
 (has-globalspecialfulfillmentrequestcode :type globalSpecialFulfillmentRequestCode
             :max-cardinality 1)
 (has-packlistrequirements :type packListRequirements
             :max-cardinality 1)
 (has-specialhandlinginstruction :type specialHandlingInstruction
             :max-cardinality 1)))

(def-class carrierInformation()
((has-accountidentifier :type accountIdentifier
             :max-cardinality 1)
 (has-globalcarriercode :type globalCarrierCode
             :max-cardinality 1)))

(def-class accountIdentifier()
((has-proprietaryreferenceidentifier :type proprietaryReferenceIdentifier
             :max-cardinality 1)))
 
(def-class globalCarrierCode(string))

(def-class globalFreeOnBoardCode(string))

(def-class globalShipmentTermsCode(string))

(def-class globalShippingServiceLevelCode(string))

(def-class globalSpecialFulfillmentRequestCode(string))

(def-class packListRequirements()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class specialHandlingInstruction()
((has-globalspecialhandlingcode :type globalSpecialHandlingCode
             :max-cardinality 1)
 (has-specialhandlingtext :type specialHandlingText
             :max-cardinality 1)))

(def-class globalSpecialHandlingCode(string))

(def-class specialHandlingText)
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class productLineItem()
((has-freeformtext :type freeFormText
             :max-cardinality 1)
 (has-contractinformation :type contractInformation
             :max-cardinality 1)
 (has-countryoforigin :type countryOfOrigin
             :max-cardinality 1)
 (has-customerinformation :type customerInformation
             :max-cardinality 1)
 (has-documentreference :type documentReference
             :max-cardinality 1)
 (has-expeditereferenceidentifier :type expediteReferenceIdentifier
             :max-cardinality 1)
 (has-globalproductunitofmeasurecode :type globalProductUnitOfMeasureCode
             :max-cardinality 1)
 (has-globalpurchaseorderfillprioritycode :type globalPurchaseOrderFillPriorityCode
             :max-cardinality 1)
 (has-installat :type installAt
             :max-cardinality 1)
 (has-isdropship :type isDropShip
             :max-cardinality 1)
 (has-linenumber :type lineNumber
             :max-cardinality 1)
 (has-orderquantity :type orderQuantity
             :max-cardinality 1)
 (has-ordershippinginformation :type orderShippingInformation
             :max-cardinality 1)
 (has-productidentification :type productIdentification
             :max-cardinality 1)
 (has-productsublineitem :type productSubLineItem
             :max-cardinality 1)
 (has-proprietaryinformation :type proprietaryInformation
             :max-cardinality 1)
 (has-requestedevent :type requestedEvent
             :max-cardinality 1)
 (has-requestedshipfrom :type requestedShipFrom
             :max-cardinality 1)
 (has-requestedunitprice :type requestedUnitPrice
             :max-cardinality 1)
 (has-shipto :type shipTo
             :max-cardinality 1)
 (has-taxexemptstatus :type taxExemptStatus
             :max-cardinality 1)
 (has-totallineitemamount :type totalLineItemAmount
             :max-cardinality 1)))

(def-class countryOfOrigin()
((has-globalcountrycode :type globalCountryCode
             :max-cardinality 1)))

(def-class customerInformation()
((has-customerprojectidentifier :type customerProjectIdentifier
             :max-cardinality 1)
 (has-globalcustomertypecode :type globalCustomerTypeCode
             :max-cardinality 1)
 (has-partnerdescription :type partnerDescription
             :max-cardinality 1)))

(def-class customerProjectIdentifier()
((has-proprietaryreferenceidentifier :type proprietaryReferenceIdentifier
             :max-cardinality 1)

(def-class globalCustomerTypeCode(string))

(def-class nationalBusinessTaxIdentifier
((has-businesstaxidentifier :type businessTaxIdentifier
             :max-cardinality 1)
 (has-globalcountrycode :type globalCountryCode
             :max-cardinality 1)))

(def-class businessTaxIdentifier(string))

(def-class expediteReferenceIdentifier()
 (has-proprietaryreferenceidentifier :type proprietaryReferenceIdentifier
             :max-cardinality 1)))

(def-class globalProductUnitOfMeasureCode(string))

(def-class orderQuantity()
 (has-requestedquantity :type requestedQuantity
             :max-cardinality 1)))

(def-class productIdentification()
((has-globalproductidentifier :type globalProductIdentifier
             :max-cardinality 1)
 (has-partnerproductidentification :type partnerProductIdentification
             :max-cardinality 1)))

(def-class requestedQuantity()
((has-productquantity :type productQuantity
             :max-cardinality 1)))

(def-class productQuantity(float))

(def-class partnerProductIdentification()
((has-globalpartnerclassificationcode :type globalPartnerClassificationCode
             :max-cardinality 1)
 (has-proprietaryproductidentifier :type proprietaryProductIdentifier
             :max-cardinality 1)
 (has-revisionidentifier :type revisionIdentifier
             :max-cardinality 1)))

(def-class globalProductIdentifier(string))

(def-class proprietaryProductIdentifier(string))

(def-class revisionIdentifier()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class productSubLineItem()
((has-freeformtext :type freeFormText
             :max-cardinality 1)
 (has-contractinformation :type contractInformation
             :max-cardinality 1)
 (has-countryoforigin :type countryOfOrigin
             :max-cardinality 1)
 (has-customerinformation :type customerInformation
             :max-cardinality 1)
 (has-expeditereferenceidentifier :type expediteReferenceIdentifier
             :max-cardinality 1)
 (has-globalproductunitofmeasurecode :type globalProductUnitOfMeasureCode
             :max-cardinality 1)
 (has-globalpurchaseorderfillprioritycode :type globalPurchaseOrderFillPriorityCode
             :max-cardinality 1)
 (has-installat :type installAt
             :max-cardinality 1)
 (has-isdropship :type isDropShip
             :max-cardinality 1)
 (has-orderquantity :type orderQuantity
             :max-cardinality 1)
 (has-ordershippinginformation :type orderShippingInformation
             :max-cardinality 1)
 (has-proprietaryinformation :type proprietaryInformation
             :max-cardinality 1)
 (has-requestedevent :type requestedEvent
             :max-cardinality 1)
 (has-requestedshipfrom :type requestedShipFrom
             :max-cardinality 1)
 (has-requestedunitprice :type requestedUnitPrice
             :max-cardinality 1)
 (has-shipto :type shipTo
             :max-cardinality 1)
 (has-sublineitem :type subLineItem
             :max-cardinality 1)))

(def-class proprietaryInformation()
((has-freeformtext :type freeFormText
             :max-cardinality 1)))

(def-class requestedEvent()
((has-transportationevent :type transportationEvent
             :max-cardinality 1)))

;; dateAndTime from date-time
(def-class transportationEvent()
((has-begintime :type beginTime
             :max-cardinality 1)
 (has-datestamp :type dateAndTime
             :max-cardinality 1)
 (has-endtime :type endTime
             :max-cardinality 1)
 (has-globaltransporteventcode :type globalTransportEventCode
             :max-cardinality 1)))

;; dateAndTime from date-time
(def-class beginTime()
((has-timestamp :type dateAndTime
             :max-cardinality 1)))

;; dateAndTime from date-time
(def-class endTime()
((has-timestamp :type dateAndTime
             :max-cardinality 1)))

(def-class globalTransportEventCode(string))

(def-class requestedShipFrom()
((has-physicaladdress :type physicalAddress
             :max-cardinality 1)))

(def-class requestedUnitPrice()
((has-financialamount :type financialAmount
             :max-cardinality 1)))

(def-class financialAmount()
((has-globalcurrencycode :type cu:currency
             :max-cardinality 1)
 (has-globalmonetaryamounttypecode :type globalMonetaryAmountTypeCode
             :max-cardinality 1)
 (has-invoicechargetypecode :type invoiceChargeTypeCode
             :max-cardinality 1)
 (has-monetaryamount :type monetaryAmount
             :max-cardinality 1)))

(def-class globalMonetaryAmountTypeCode(string))

(def-class invoiceChargeTypeCode(string))

(def-class monetaryAmount(float))

(def-class shipTo()
((has-partnerdescription :type partnerDescription
             :max-cardinality 1)))

(def-class subLineItem()
((has-linenumber :type lineNumber
             :max-cardinality 1)))

(def-class taxExemptStatus()
((has-istaxexempt :type isTaxExempt
             :max-cardinality 1)
 (has-taxexemption :type taxExemption
             :max-cardinality 1)))

(def-class isTaxExempt()
((has-affirmationindicator :type affirmationIndicator
             :max-cardinality 1)))

(def-class taxExemption()
((has-globaltaxexemptioncode :type globalTaxExemptionCode
             :max-cardinality 1)
 (has-taxexemptioncertificationidentifier :type taxExemptionCertificationIdentifier
             :max-cardinality 1)))

(def-class globalTaxExemptionCode(string))

(def-class taxExemptionCertificationIdentifier()
((has-proprietaryreferenceidentifier :type ProprietaryReferenceIdentifier
             :max-cardinality 1)))

(def-class totalLineItemAmount()
((has-financialamount :type financialAmount
             :max-cardinality 1)))

(def-class secondaryBuyer()
((has-partnerdescription :type partnerDescription
             :max-cardinality 1)
 (has-secondarybuyerpurchaseorderidentifier :type secondaryBuyerPurchaseOrderIdentifier
             :max-cardinality 1)))

(def-class secondaryBuyerPurchaseOrderIdentifier()
((has-proprietarydocumentidentifier :type proprietaryDocumentIdentifier
             :max-cardinality 1)))

(def-class totalAmount()
 (has-financialamount :type financialAmount
             :max-cardinality 1)))

;; dateAndTime from date-time
(def-class thisDocumentGenerationDateTime()
((has-datetimestamp :type dateAndTime
             :max-cardinality 1)))

(def-class thisDocumentIdentifier()
((has-proprietarydocumentidentifier :type proprietaryDocumentIdentifier
             :max-cardinality 1)))
