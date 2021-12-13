package nl.surfnet.safnari

import nl.surfnet.nsiv2.persistence.MessageStore

class SafnariMessageStore(db: play.api.db.Database, app: play.api.Application) extends MessageStore[Message](db)(app, MessagePersistence.MessageToMessageData)
