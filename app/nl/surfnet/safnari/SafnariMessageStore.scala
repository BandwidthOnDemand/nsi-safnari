package nl.surfnet.safnari

import nl.surfnet.nsiv2.persistence.MessageStore

class SafnariMessageStore(db: play.api.db.Database) extends MessageStore[Message](db)(MessagePersistence.MessageToMessageData)
