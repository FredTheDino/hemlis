paymentVouchers exchangeRateToSEK payment invoiceDate { invoiceId, transactionKind, balance } creditInvoices =
  let
    bookkeepingDate = foo

    creditInvoiceVouchers
      :: { amount :: MoneyString, invoiceId :: GivenNumber }
      -> Veither _ { }
    creditInvoiceVouchers c =
      ado
        Veither.assert @"errFortnoxCreditInvoiceWithPositiveAmount" c (MoneyString.isStrictlyNegative c.amount)
        in
          { }
  in
  Veither.do
    amountInForeignCurrency <- a

    vouchersForCreditInvoices <- a

    voucher <-
      case a of
        CC.Sell ->
          pure $ Left
            { invoiceId }

        CC.Buy ->
          ado
            amountInSEK <- a
            in
              Right { invoiceId }
    pure { voucher }

--+ expected stdout:
--+ B
--+ 103 of 103
--+ ===
--+ Unexpected(Known(0, 15, 0), Some(Lower("paymentVouchers")), "T::Lower(\"module\")")
--+ >>>>>
--+ paymentVouchers exchangeRateToSEK payment invoiceDate { invoiceId, transactionKind, balance } creditInvoices =
--+ <<<<<<
--+ ===

