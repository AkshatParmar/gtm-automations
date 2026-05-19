# COBOL Banking Transaction System - Modernization Examples

This directory contains synthetic examples of a legacy COBOL-based banking
transaction processing system, representative of what large financial
institutions have run on IBM mainframes for decades.

## System Overview

The system models a core banking platform with:

| Module | Source File | Description |
|--------|-------------|-------------|
| Account Master | `ACCTMSTR.cbl` | Account CRUD, balance inquiries, status management |
| Transaction Engine | `TRANPROC.cbl` | Deposit, withdrawal, transfer processing with journaling |
| Batch Settlement | `BATCHSET.cbl` | End-of-day batch settlement, interest calculation, reporting |
| Customer Info | `CUSTINFO.cbl` | Customer record management and KYC data |
| Audit Trail | `AUDTLOG.cbl` | Regulatory audit logging and compliance reporting |

## File Organization (VSAM / ISAM)

- `ACCT-MASTER-FILE` - Indexed by account number (10-digit)
- `TRANS-JOURNAL-FILE` - Sequential transaction journal
- `CUST-MASTER-FILE` - Indexed by customer ID
- `DAILY-SETTLE-FILE` - Daily settlement output
- `AUDIT-LOG-FILE` - Sequential audit log

## Typical Mainframe Environment

- **Platform:** IBM z/OS, CICS for online transactions, JCL for batch
- **Data:** VSAM KSDS files, DB2 tables, IMS segments
- **Communication:** MQ Series for inter-system messaging
- **Security:** RACF for access control

## Modernization Target

See [MODERNIZATION-PLAN.md](../MODERNIZATION-PLAN.md) for the full
Java/Spring Boot migration strategy.
