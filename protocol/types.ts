// x. namespace is for chat messages transmitted inside SMP agent MSG
type MemberMessageType =
  | "x.grp.info" // group profile information or update
  | "x.grp.off" // disable group
  | "x.grp.del" // group deleted
  | "x.grp.mem.new" // new group member
  | "x.grp.mem.acl" // group member permissions (ACL)
  | "x.grp.mem.leave" // group member left
  | "x.grp.mem.off" // suspend group member
  | "x.grp.mem.on" // enable group member
  | "x.grp.mem.del" // group member removed

type ProfileMessageType =
  | "x.info" // profile information or update
  | "x.info.grp" // information about group in profile
  | "x.info.con" // information about contact in profile

type NotificationMessageType = "x.msg.read"

type OpenConnMessageType =
  | "x.open.grp" // open invitation to the group
  | "x.open.con" // open invitation to the contact

type ContentMessageType =
  | "x.msg.new" // new message
  | "x.msg.append" // additional part of the message
  | "x.msg.del" // delete message
  | "x.msg.update" // update message
  | "x.msg.fwd" // forward message
  | "x.msg.reply" // reply to message

// TODO namespace for chat messages transmitted as other agent messages

type DirectMessageType =
  | ProfileMessageType
  | NotificationMessageType
  | OpenConnMessageType
  | ContentMessageType

type GroupMessageType = MemberMessageType | DirectMessageType

type ContentType =
  | "c.text"
  | "c.html"
  | "c.image"
  | "c.audio"
  | "c.video"
  | "c.doc"
  | "c.sticker"
  | "c.file"
  | "c.link"
  | "c.form"
  | "c.poll"
  | "c.applet"

// the type of message data transmitted inside SMP agent MSG
interface MessageData<T extends GroupMessageType> {
  type: T
  sent: Date
  data: unknown
}

interface DirectMessageData<T extends DirectMessageType> extends MessageData<T> {}

interface GroupMessageData<T extends GroupMessageType> extends MessageData<T> {
  msgId: number
  parents: ParentMessage[]
}

interface ParentMessage {
  memberId: Uint8Array
  msgId: number
  msgHash: Uint8Array
}
