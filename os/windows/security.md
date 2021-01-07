Nearly all sharable objects can be protected.
---

Nearly any object created with a `CreateXYZ` function has a security attributes parameter `SECURITY_ATTRIBUTES`, including files, processes, threads, events, semaphores, named pipes and so on.

```c
typedef struct _SECURITY_ATTRIBUTES {
  DWORD  nLength;
  LPVOID lpSecurityDescriptor; // describes the object's owner and determines which users are allowed or denied various rights
  BOOL   bInheritHandle;
} SECURITY_ATTRIBUTES, *PSECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;
```

An individual process is identified by its /access token/, which specifies the owning user and group membership. The kernel decides whether the processs can access an object by combining the token and the information from the security descriptor of the object.

# Access Control Lists

A set of access control entries: for access allowed and for access denied.

Each ACE contains a /SID/ and an /access mask/, which specifies rights to be granted or denied to the user or group specified by the SID

```
 +---------------+                        +-----------------+
 |    Process    |                        |   Object        |
 +-------+-------+                        +--------+--------+
         |                                         |
         |                                         |
         v                                         v
+--------+--------+                    +-----------+------------+
|                 |  Access            |                        |
|    User SID     |  Token             |       Owner SID        |  Security
|    Group SID    |                    |                        |  Descriptor
|                 |                    |       Group SID        |
+-----------------+                    |                        |
                                       +-----------+------------+
                                                   |
                                                   |
                                                   |
                                   +---------------v------------+
                                   |                            | Discretionary ACL
                                   |       ACE  (Denied)        |
                                   |                            |
                                   |       ACE  (Allowed)       |
                                   |                            |
                                   |       ACE  (Allowed)       |
                                   |                            |
                                   |                            |
                                   |                            |
                                   |                            |
                                   +----------------------------+
```

Entries for access denied comes first in the ACL. The first entry that specifically grants or denies the requested service is decisive.

An object gets its security descriptor at creation time. A process requests access to the object when it asks for a handle. The handle request contains the desired access.

# The Security Descriptor

A security descriptor contains:

1. the owner's security identifier (SID)

2. the group SID

3. Discretionary access control list (DACL): a list of entries explicitly granting and denying access rights.

4. System ACL


## Creting the Security Descriptor

```c
BOOL InitializeSecurityDescriptor(PSECURITY_DESCRIPTOR pSecurityDescriptor, DWORD dwRevision);
```


## Security Identifiers (SIDs)

Windows uses SIDS to identify users and grops.

```cpp
BOOL LookupAccountNameW(
  LPCWSTR       lpSystemName,
  LPCWSTR       lpAccountName,
  PSID          Sid,
  LPDWORD       cbSid,
  LPWSTR        ReferencedDomainName,
  LPDWORD       cchReferencedDomainName,
  PSID_NAME_USE peUse
);

BOOL LookupAccountSidW(
  LPCWSTR       lpSystemName,
  PSID          Sid,
  LPWSTR        Name,
  LPDWORD       cchName,
  LPWSTR        ReferencedDomainName,
  LPDWORD       cchReferencedDomainName,
  PSID_NAME_USE peUse
);

BOOL GetUserNameW(
  LPWSTR  lpBuffer,
  LPDWORD pcbBuffer
);
```

Once the SIDs are known, they can be used to fill in the security descriptor structure.

```cpp
BOOL SetSecurityDescriptorOwner(
  PSECURITY_DESCRIPTOR pSecurityDescriptor,
  PSID                 pOwner,
  BOOL                 bOwnerDefaulted
);

BOOL SetSecurityDescriptorGroup(
  PSECURITY_DESCRIPTOR pSecurityDescriptor,
  PSID                 pGroup,
  BOOL                 bGroupDefaulted
);
```

## Managing ACLs

### Create an ACL

The program must provide a buffer to serve as the ACL. 1KB is more than adequate for most purposes.

```cpp
BOOL InitializeAcl(
  PACL  pAcl,
  DWORD nAclLength,
  DWORD dwAclRevision
);
```

### Adding ACEs

The predefined mask values will vary by the object type.

```cpp
BOOL AddAccessAllowedAce(
  PACL  pAcl,
  DWORD dwAceRevision,
  DWORD AccessMask,
  PSID  pSid
);

BOOL AddAccessDeniedAce(
  PACL  pAcl,
  DWORD dwAceRevision,
  DWORD AccessMask,
  PSID  pSid
);
```

### Adding an ACL to a security descriptor

```cpp
BOOL SetSecurityDescriptorDacl(
  PSECURITY_DESCRIPTOR pSecurityDescriptor,
  BOOL                 bDaclPresent,
  PACL                 pDacl,
  BOOL                 bDaclDefaulted
);
```

# Reading and Changing Security Descriptors

`GeetFileSecurity()`, `SetFileSecurity()`, `GetSecurityDescriptorOwner()`, `GetSecurityDescrptorGroup()`

```cpp
BOOL GetSecurityDescriptorDacl(
  PSECURITY_DESCRIPTOR pSecurityDescriptor,
  LPBOOL               lpbDaclPresent,
  PACL                 *pDacl,
  LPBOOL               lpbDaclDefaulted
);
```

To interpret an ACL

```cpp
BOOL GetAclInformation(
  PACL                  pAcl,
  LPVOID                pAclInformation,
  DWORD                 nAclInformationLength,
  ACL_INFORMATION_CLASS dwAclInformationClass
);

BOOL GetAce(
  PACL   pAcl,
  DWORD  dwAceIndex,
  LPVOID *pAce
);
```

# Kernel and Private Object Security

Object such as processes, threads, and mutexes are kernel objects. `GetKernelObjectSecurity()`, `SetKernelObjectSecurity()` with appropriate privileges.

# Additional Security Features

1. `DeleteAce()`

2. absolute and self-relative security descriptors

3. System ACLs: specify which object accesses should be logged.
