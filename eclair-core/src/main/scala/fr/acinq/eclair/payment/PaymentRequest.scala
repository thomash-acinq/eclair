/*
 * Copyright 2019 ACINQ SAS
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

import fr.acinq.bitcoin.ByteVector32
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.payment.Bolt11Invoice.long2bits
import fr.acinq.eclair.payment.PaymentRequest.{ExtraHop, PaymentRequestFeatures}
import fr.acinq.eclair.{CltvExpiryDelta, FeatureSupport, Features, MilliSatoshi, NodeParams, ShortChannelId, TimestampSecond}
import scodec.bits.{BitVector, ByteVector}

trait PaymentRequest {
  val amount: Option[MilliSatoshi]

  val timestamp: TimestampSecond

  val nodeId: PublicKey

  val paymentHash: ByteVector32

  val paymentSecret: Option[ByteVector32]

  val description: Either[String, ByteVector32]

  val routingInfo: Seq[Seq[ExtraHop]]

  val relativeExpiry: Long

  val minFinalCltvExpiryDelta: Option[CltvExpiryDelta]

  val features: Features

  def isExpired: Boolean =  timestamp + relativeExpiry <= TimestampSecond.now()

  def write: String
}

object PaymentRequest {

  /**
   * Extra hop contained in RoutingInfoTag
   *
   * @param nodeId                    start of the channel
   * @param shortChannelId            channel id
   * @param feeBase                   node fixed fee
   * @param feeProportionalMillionths node proportional fee
   * @param cltvExpiryDelta           node cltv expiry delta
   */
  case class ExtraHop(nodeId: PublicKey, shortChannelId: ShortChannelId, feeBase: MilliSatoshi, feeProportionalMillionths: Long, cltvExpiryDelta: CltvExpiryDelta)


  /**
   * Features supported or required for receiving this payment.
   */
  case class PaymentRequestFeatures(bitmask: BitVector) {
    lazy val features: Features = Features(bitmask)
    lazy val allowMultiPart: Boolean = features.hasFeature(Features.BasicMultiPartPayment)
    lazy val allowPaymentSecret: Boolean = features.hasFeature(Features.PaymentSecret)
    lazy val requirePaymentSecret: Boolean = features.hasFeature(Features.PaymentSecret, Some(FeatureSupport.Mandatory))
    lazy val allowTrampoline: Boolean = features.hasFeature(Features.TrampolinePayment)

    def toByteVector: ByteVector = features.toByteVector

    def areSupported(nodeParams: NodeParams): Boolean = nodeParams.features.areSupported(features)

    override def toString: String = s"Features(${bitmask.toBin})"
  }

  object PaymentRequestFeatures {
    def apply(features: Int*): PaymentRequestFeatures = PaymentRequestFeatures(long2bits(features.foldLeft(0L) {
      case (current, feature) => current + (1L << feature)
    }))
  }

  def fastHasExpired(input: String): Boolean =
    Bolt11Invoice.fastHasExpired(input)

  def read(input: String): PaymentRequest =
    Bolt11Invoice.read(input)
}