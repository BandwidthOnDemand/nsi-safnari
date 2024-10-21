/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._12.connection.types.ReservationConfirmCriteriaType

import nl.surfnet.nsiv2.messages.{given, *}

trait InitialReserveAlgorithm:
  def forSegments(segments: ComputedPathSegments): InitialReserveAlgorithm

  def nextSegments: Map[CorrelationId, ComputedSegment]
  def clearNextSegments: InitialReserveAlgorithm

  def reserveConfirmed(
      correlationId: CorrelationId,
      criteria: ReservationConfirmCriteriaType
  ): InitialReserveAlgorithm
object InitialReserveAlgorithm:
  def forAlgorithm(algorithm: PathComputationAlgorithm): InitialReserveAlgorithm = algorithm match
    case PathComputationAlgorithm.CHAIN | PathComputationAlgorithm.TREE =>
      SimultaneousInitialReserveAlgorithm(Map.empty)
    case PathComputationAlgorithm.SEQUENTIAL =>
      SequentialInitialReserveAlgorithm(Map.empty, None, Seq.empty)

  private case class SimultaneousInitialReserveAlgorithm(
      nextSegments: Map[CorrelationId, ComputedSegment]
  ) extends InitialReserveAlgorithm:
    def forSegments(segments: ComputedPathSegments): SimultaneousInitialReserveAlgorithm =
      copy(nextSegments = segments.toMap)
    def clearNextSegments: InitialReserveAlgorithm = copy(nextSegments = Map.empty)
    def reserveConfirmed(
        correlationId: CorrelationId,
        criteria: ReservationConfirmCriteriaType
    ): InitialReserveAlgorithm = clearNextSegments

  private case class SequentialInitialReserveAlgorithm(
      nextSegments: Map[CorrelationId, ComputedSegment],
      outstandingSegment: Option[(CorrelationId, ComputedSegment)],
      remainingSegments: ComputedPathSegments
  ) extends InitialReserveAlgorithm:

    def forSegments(segments: ComputedPathSegments): SequentialInitialReserveAlgorithm = copy(
      nextSegments = segments.headOption.toMap,
      outstandingSegment = segments.headOption,
      remainingSegments = segments.drop(1)
    )

    def reserveConfirmed(
        correlationId: CorrelationId,
        criteria: ReservationConfirmCriteriaType
    ): InitialReserveAlgorithm =
      assert(
        outstandingSegment.exists(_._1 == correlationId),
        s"reservation confirm $correlationId does not match any outstanding request: $outstandingSegment"
      )
      remainingSegments match
        case Seq() =>
          SequentialInitialReserveAlgorithm(Map.empty, None, Seq.empty)
        case next +: remaining =>
          val service = criteria.pointToPointService.getOrElse {
            throw new RuntimeException("P2PService is missing")
          }

          val nextSegment = service.destStp.vlan
            .map { vlan =>
              next.copy(_2 = next._2.copy(serviceType = next._2.serviceType.copy(service =
                val nextService = next._2.serviceType.service.shallowCopy
                nextService.withSourceSTP(
                  nextService.sourceStp.withLabel("vlan", vlan.toString()).toString()
                )
              )))
            }
            .getOrElse(next)

          SequentialInitialReserveAlgorithm(Map(nextSegment), Some(nextSegment), remaining)
      end match
    end reserveConfirmed

    def clearNextSegments: InitialReserveAlgorithm = copy(nextSegments = Map.empty)
  end SequentialInitialReserveAlgorithm
end InitialReserveAlgorithm
