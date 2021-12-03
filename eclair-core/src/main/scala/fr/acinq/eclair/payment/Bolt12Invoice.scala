/*
 * Copyright 2021 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.payment

import fr.acinq.bitcoin.{ByteVector32, Crypto}
import fr.acinq.eclair.payment.PaymentRequest.ExtraHop
import fr.acinq.eclair.wire.protocol.OfferCodecs.{encodeBech32, invoiceTlvCodec}
import fr.acinq.eclair.wire.protocol.Offers._
import fr.acinq.eclair.wire.protocol.TlvStream
import fr.acinq.eclair.{CltvExpiryDelta, Features, MilliSatoshi}


case class Bolt12Invoice(records: TlvStream[InvoiceTlv]) extends PaymentRequest {

  import Bolt12Invoice._

  override val amount: Option[MilliSatoshi] = records.get[Amount].map(_.amount)

  override val nodeId: Crypto.PublicKey = records.get[NodeId].get.nodeId

  override val paymentHash: ByteVector32 = records.get[PaymentHash].get.hash

  override val paymentSecret: Option[ByteVector32] = None

  override val description: Either[String, ByteVector32] = records.get[Description].map(_.description).map(Left(_)).getOrElse(Right(paymentHash))

  override val routingInfo: Seq[Seq[ExtraHop]] = ???

  override val relativeExpiry: Long = records.get[RelativeExpiry].map(_.seconds).getOrElse(DEFAULT_EXPIRY_SECONDS)

  override val minFinalCltvExpiryDelta: Option[CltvExpiryDelta] = records.get[Cltv].map(_.minFinalCltvExpiry)

  override val features: Features = records.get[FeaturesTlv].map(_.features).getOrElse(Features.empty)

  override def write: String = {
    encodeBech32(invoiceTlvCodec, records).require
  }
}

object Bolt12Invoice {
  val DEFAULT_EXPIRY_SECONDS: Long = 7200
}


