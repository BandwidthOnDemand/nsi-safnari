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
