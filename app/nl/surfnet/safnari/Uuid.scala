/*
 * Copyright (c) 2012, 2013, 2014, 2015 SURFnet BV
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

import java.security.SecureRandom
import java.util.UUID
import scala.util.Random

/**
 * Various (thread-safe) UUID generators. The random/deterministic UUID generator
 * uses a deterministic random number generator which is not appropriate for security-
 * sensitive applications. Fortunately, we only use the UUIDs for generating message
 * correlatio ids, which should not be security-sensitive.
 */
object Uuid {
  def randomUuidGenerator(): () => UUID = deterministicUuidGenerator(new SecureRandom().nextLong())

  def deterministicUuidGenerator(seed: Long): () => UUID = {
    val random = new Random(seed)
    () => {
      val mostSig = (random.nextLong() & ~0x000000000000f000L) | 0x0000000000004000L
      val leastSig = (random.nextLong() & ~0xc000000000000000L) | 0x8000000000000000L
      new UUID(mostSig, leastSig)
    }
  }

  def mockUuidGenerator(start: Long = 1): () => UUID = {
    val lock = new Object
    var next = start
    () => lock.synchronized {
      val result = new UUID(0, next)
      next += 1
      result
    }
  }
}
