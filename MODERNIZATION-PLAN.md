# COBOL to Java Modernization Plan

## Executive Summary

This document outlines the strategy for migrating a legacy COBOL-based banking
transaction processing system (5 programs, ~3,000 LOC) from an IBM z/OS mainframe
environment to a modern Java 21 / Spring Boot 3.x microservices architecture running
on cloud infrastructure.

---

## 1. Current State Assessment

### 1.1 System Inventory

| Program | LOC | Function | Complexity | Priority |
|---------|-----|----------|------------|----------|
| ACCTMSTR.cbl | ~280 | Account master CRUD | Medium | P0 - Core |
| TRANPROC.cbl | ~340 | Transaction processing (deposit/withdrawal/transfer) | High | P0 - Core |
| BATCHSET.cbl | ~260 | End-of-day settlement & interest calculation | Medium | P1 |
| CUSTINFO.cbl | ~210 | Customer records & KYC management | Medium | P1 |
| AUDTLOG.cbl | ~280 | Audit trail & BSA/AML compliance reporting | Medium | P2 |

### 1.2 Technology Stack (Current)

| Layer | Technology |
|-------|------------|
| Language | COBOL 85 (IBM Enterprise COBOL) |
| Platform | IBM z/OS, CICS TS for online, JCL for batch |
| Data Storage | VSAM KSDS (indexed), VSAM ESDS (sequential) |
| Communication | IBM MQ Series (assumed for inter-system) |
| Security | RACF |
| Scheduling | JES2 / CA-7 |

### 1.3 Key Technical Challenges

- **COMP-3 packed decimal arithmetic** — Must preserve exact precision in Java
  using `BigDecimal`; `double`/`float` are never acceptable for financial data.
- **VSAM file semantics** — Indexed file READ/WRITE/REWRITE/DELETE maps to
  database CRUD but with subtle locking differences.
- **Two-phase transfer logic** — TRANPROC.cbl implements manual rollback for
  failed transfers; needs proper `@Transactional` boundaries.
- **Batch settlement timing** — BATCHSET runs at close-of-business with
  exclusive file access; batch window constraints change in a database world.
- **Regulatory compliance** — AUDTLOG implements BSA CTR ($10K threshold) and
  structuring detection (SAR); audit trail must be preserved exactly.

---

## 2. Target Architecture

### 2.1 Technology Stack (Target)

| Layer | Technology | Rationale |
|-------|------------|-----------|
| Language | Java 21 (LTS) | Industry standard, strong banking ecosystem |
| Framework | Spring Boot 3.3 | Production-grade, extensive enterprise support |
| API | REST + OpenAPI 3.0 | Replaces CICS screens/COMMAREA |
| Database | PostgreSQL 16 | ACID compliance, JSON support, cost-effective |
| ORM | Spring Data JPA / Hibernate 6 | Replaces VSAM file I/O |
| Batch | Spring Batch 5 | Replaces JCL batch jobs |
| Messaging | Apache Kafka / Spring Cloud Stream | Replaces MQ Series |
| Security | Spring Security 6 + OAuth2 | Replaces RACF |
| Monitoring | Micrometer + Prometheus + Grafana | Replaces mainframe SMF records |
| CI/CD | GitHub Actions + ArgoCD | Replaces manual JCL deployment |
| Container | Docker + Kubernetes | Cloud-native deployment |

### 2.2 Service Decomposition

```
                    +------------------+
                    |   API Gateway    |
                    +--------+---------+
                             |
          +------------------+------------------+
          |                  |                  |
+---------v------+ +---------v------+ +--------v--------+
| Account        | | Transaction    | | Customer        |
| Service        | | Service        | | Service         |
| (ACCTMSTR)     | | (TRANPROC)     | | (CUSTINFO)      |
+--------+-------+ +--------+-------+ +--------+--------+
         |                  |                  |
         +------------------+------------------+
                            |
                   +--------v--------+
                   |   PostgreSQL    |
                   +-----------------+
                            |
          +------------------+------------------+
          |                                     |
+---------v----------+            +-------------v------+
| Settlement Batch   |            | Audit & Compliance |
| Service (BATCHSET) |            | Service (AUDTLOG)  |
+---------+----------+            +-------------+------+
          |                                     |
          +-----------> Kafka <-----------------+
```

### 2.3 Database Schema Design

#### accounts
```sql
CREATE TABLE accounts (
    account_num     VARCHAR(10) PRIMARY KEY,
    customer_id     VARCHAR(12) NOT NULL REFERENCES customers(customer_id),
    account_type    VARCHAR(2)  NOT NULL CHECK (account_type IN ('CK','SV','MM','CD','LN')),
    status          VARCHAR(1)  NOT NULL DEFAULT 'A' CHECK (status IN ('A','F','C','D')),
    balance         NUMERIC(15,2) NOT NULL DEFAULT 0,
    available_bal   NUMERIC(15,2) NOT NULL DEFAULT 0,
    hold_amount     NUMERIC(15,2) NOT NULL DEFAULT 0,
    overdraft_limit NUMERIC(11,2) NOT NULL DEFAULT 0,
    interest_rate   NUMERIC(9,6)  NOT NULL DEFAULT 0,
    open_date       DATE NOT NULL,
    close_date      DATE,
    last_trans_ts   TIMESTAMP NOT NULL,
    branch_code     VARCHAR(6),
    officer_id      VARCHAR(8),
    currency_code   VARCHAR(3) NOT NULL DEFAULT 'USD',
    daily_trans_count INT NOT NULL DEFAULT 0,
    daily_trans_total NUMERIC(15,2) NOT NULL DEFAULT 0,
    mtd_interest    NUMERIC(11,2) NOT NULL DEFAULT 0,
    ytd_interest    NUMERIC(13,2) NOT NULL DEFAULT 0,
    created_at      TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at      TIMESTAMP NOT NULL DEFAULT NOW()
);
```

#### transactions
```sql
CREATE TABLE transactions (
    trans_id        UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    trans_type      VARCHAR(2) NOT NULL,
    account_num     VARCHAR(10) NOT NULL REFERENCES accounts(account_num),
    related_acct    VARCHAR(10),
    amount          NUMERIC(15,2) NOT NULL,
    balance_before  NUMERIC(15,2) NOT NULL,
    balance_after   NUMERIC(15,2) NOT NULL,
    trans_ts        TIMESTAMP NOT NULL DEFAULT NOW(),
    operator_id     VARCHAR(8),
    terminal_id     VARCHAR(8),
    status          VARCHAR(2) NOT NULL DEFAULT 'OK',
    description     VARCHAR(40),
    reference_num   VARCHAR(20),
    created_at      TIMESTAMP NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_trans_account ON transactions(account_num, trans_ts);
```

#### customers
```sql
CREATE TABLE customers (
    customer_id     VARCHAR(12) PRIMARY KEY,
    ssn             VARCHAR(9) UNIQUE,
    first_name      VARCHAR(25) NOT NULL,
    middle_init     VARCHAR(1),
    last_name       VARCHAR(30) NOT NULL,
    suffix          VARCHAR(5),
    addr_line_1     VARCHAR(35),
    addr_line_2     VARCHAR(35),
    city            VARCHAR(25),
    state           VARCHAR(2),
    zip_code        VARCHAR(10),
    country         VARCHAR(3) DEFAULT 'USA',
    home_phone      VARCHAR(15),
    work_phone      VARCHAR(15),
    mobile_phone    VARCHAR(15),
    email           VARCHAR(50),
    dob             DATE,
    customer_type   VARCHAR(1) NOT NULL DEFAULT 'I',
    status          VARCHAR(1) NOT NULL DEFAULT 'A',
    kyc_id_type     VARCHAR(2),
    kyc_id_number   VARCHAR(20),
    kyc_verified    BOOLEAN NOT NULL DEFAULT FALSE,
    kyc_verify_date DATE,
    risk_rating     VARCHAR(1) DEFAULT 'L',
    open_date       DATE NOT NULL,
    branch_code     VARCHAR(6),
    officer_id      VARCHAR(8),
    created_at      TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at      TIMESTAMP NOT NULL DEFAULT NOW()
);
```

#### audit_log
```sql
CREATE TABLE audit_log (
    log_id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    event_type      VARCHAR(3) NOT NULL,
    severity        VARCHAR(1) NOT NULL DEFAULT 'I',
    operator_id     VARCHAR(8),
    terminal_id     VARCHAR(8),
    account_num     VARCHAR(10),
    customer_id     VARCHAR(12),
    amount          NUMERIC(15,2),
    description     VARCHAR(80),
    ip_address      VARCHAR(45),
    created_at      TIMESTAMP NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_audit_date ON audit_log(created_at);
CREATE INDEX idx_audit_account ON audit_log(account_num, created_at);
```

---

## 3. Migration Phases

### Phase 1: Foundation (Weeks 1-4)

| Task | Details | Deliverable |
|------|---------|-------------|
| Project scaffolding | Multi-module Maven/Gradle project, Spring Boot starters | Build passing |
| Database schema | Flyway migrations for all tables above | Schema deployed to dev |
| Entity mapping | JPA entities for accounts, customers, transactions, audit_log | Unit tests green |
| Repository layer | Spring Data JPA repos with custom queries | Integration tests green |
| CI/CD pipeline | GitHub Actions: build, test, Docker image, deploy to dev | Pipeline running |

### Phase 2: Core Services (Weeks 5-10)

| Task | COBOL Source | Deliverable |
|------|-------------|-------------|
| Account Service | ACCTMSTR.cbl | REST API: create, read, update, close, freeze accounts |
| Transaction Service | TRANPROC.cbl | REST API: deposit, withdraw, transfer with journaling |
| Customer Service | CUSTINFO.cbl | REST API: customer CRUD, KYC verification |
| Data migration | VSAM -> PostgreSQL | ETL scripts, data validation report |

#### Key Mapping: ACCTMSTR.cbl -> AccountService.java

| COBOL Paragraph | Java Method | Notes |
|----------------|-------------|-------|
| 3000-CREATE-ACCOUNT | `createAccount(CreateAccountRequest)` | Returns `AccountResponse` |
| 4000-READ-ACCOUNT | `getAccount(String accountNum)` | Throws `AccountNotFoundException` |
| 5000-UPDATE-ACCOUNT | `updateAccount(String, UpdateAccountRequest)` | `@Transactional`, checks status |
| 6000-CLOSE-ACCOUNT | `closeAccount(String accountNum)` | Validates zero balance |
| 7000-FREEZE-ACCOUNT | `freezeAccount(String accountNum)` | Validates active status |
| 8000-LOG-HISTORY | `logHistory(Account, String action)` | Audit event via Kafka |

#### Key Mapping: TRANPROC.cbl -> TransactionService.java

| COBOL Paragraph | Java Method | Notes |
|----------------|-------------|-------|
| 3000-VALIDATE-TRANSACTION | `validateTransaction(TransactionRequest)` | Returns validation result |
| 5000-PROCESS-DEPOSIT | `processDeposit(TransactionRequest)` | `@Transactional` |
| 5100-PROCESS-WITHDRAWAL | `processWithdrawal(TransactionRequest)` | Checks available balance + overdraft |
| 5200-PROCESS-TRANSFER | `processTransfer(TransferRequest)` | `@Transactional` atomic debit+credit |
| 6000-WRITE-JOURNAL-SUCCESS | via `TransactionRepository.save()` | Automatic within transaction |
| 4100-GENERATE-TRANS-ID | `UUID.randomUUID()` | Replaces sequential counter |

### Phase 3: Batch & Compliance (Weeks 11-14)

| Task | COBOL Source | Deliverable |
|------|-------------|-------------|
| Settlement batch | BATCHSET.cbl | Spring Batch job: interest calc, fee assessment, daily reset |
| Audit service | AUDTLOG.cbl | Streaming audit processor, CTR/SAR report generation |
| Scheduling | JCL replacement | Spring `@Scheduled` or Kubernetes CronJob |

#### Settlement Batch Design (BATCHSET.cbl -> Spring Batch)

```
Job: dailySettlementJob
  Step 1: calculateInterest
    Reader:  JpaPagingItemReader<Account> (status = ACTIVE, balance > 0)
    Processor: InterestCalculationProcessor (daily rate = annual_rate / 365)
    Writer:  JpaItemWriter<Account>
  Step 2: assessFees
    Reader:  JpaPagingItemReader<Account> (checking + low balance OR overdraft)
    Processor: FeeAssessmentProcessor ($12.50 low-bal, $35 overdraft)
    Writer:  JpaItemWriter<Account>
  Step 3: resetDailyCounters
    Tasklet: SQL UPDATE accounts SET daily_trans_count=0, daily_trans_total=0
  Step 4: generateReport
    Tasklet: Query + format settlement report
```

#### Audit / Compliance (AUDTLOG.cbl -> AuditService)

| COBOL Rule | Java Implementation |
|-----------|---------------------|
| CTR: single transaction > $10,000 | Kafka consumer triggers `CtrReportGenerator` |
| CTR: cumulative daily > $10,000 | Scheduled job queries daily totals per account |
| SAR: structuring detection (3+ transactions $9K-$10K) | Sliding window analysis in `StructuringDetector` |
| Large transaction flag > $50,000 | Real-time alert via Kafka + audit_log entry |

### Phase 4: Testing & Validation (Weeks 15-18)

| Activity | Approach |
|----------|----------|
| Unit tests | JUnit 5 + Mockito for all service methods |
| Integration tests | Testcontainers (PostgreSQL, Kafka) |
| Regression testing | Replay production VSAM data through both systems, compare results |
| Performance testing | JMeter/Gatling, target: <100ms p99 for online, settlement <30min |
| UAT | Business users validate all account/transaction flows |
| Compliance review | Audit team validates CTR/SAR output matches legacy system |

### Phase 5: Cutover (Weeks 19-20)

| Step | Description |
|------|-------------|
| Data migration | Final VSAM -> PostgreSQL ETL with validation checksums |
| Parallel run | Both systems process same transactions for 1 week |
| Reconciliation | Automated balance comparison, transaction-by-transaction diff |
| Go-live | DNS cutover, CICS routing to new REST APIs |
| Hypercare | 2-week intensive monitoring, instant rollback capability |

---

## 4. Risk Register

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| Precision loss in financial calculations | Critical | Medium | Mandate `BigDecimal` everywhere, automated precision tests |
| Hidden business rules in COBOL condition names | High | High | 88-level audit, business analyst review of every condition |
| Batch settlement window exceeded | High | Low | Spring Batch partitioning, parallel chunk processing |
| Regulatory non-compliance during migration | Critical | Medium | Parallel run with automated reconciliation |
| Data migration integrity | Critical | Medium | Checksum validation, row-count verification, balance reconciliation |
| CICS transaction semantics lost | High | Medium | Map SYNCPOINT to @Transactional, integration test all paths |

---

## 5. COBOL-to-Java Quick Reference

| COBOL | Java |
|-------|------|
| `PIC X(n)` | `String` |
| `PIC 9(n)` | `int` / `long` |
| `PIC S9(n)V9(m) COMP-3` | `BigDecimal` |
| `88-level condition` | `enum` or predicate method |
| `PERFORM paragraph` | Method call |
| `PERFORM UNTIL` | `while` loop |
| `EVALUATE TRUE` | `switch` / if-else chain |
| `MOVE` | Assignment |
| `COMPUTE ROUNDED` | `BigDecimal` with `RoundingMode.HALF_UP` |
| `STRING ... DELIMITED BY` | `String.format()` / `StringBuilder` |
| `READ file KEY IS` | `repository.findById()` |
| `WRITE record` | `repository.save()` |
| `REWRITE record` | `repository.save()` (update) |
| `DISPLAY` | `log.info()` |
| `GOBACK` | `return` |
| `FILE STATUS` | Exception handling |
| VSAM KSDS | PostgreSQL table + primary key |
| JCL batch | Spring Batch `Job` |
| CICS transaction | REST endpoint |
| RACF | Spring Security |
| MQ Series | Kafka / Spring Cloud Stream |

---

## 6. Project Structure (Target)

```
bank-modernization/
  pom.xml
  account-service/
    src/main/java/com/bank/account/
      entity/Account.java
      entity/AccountHistory.java
      repository/AccountRepository.java
      service/AccountService.java
      controller/AccountController.java
      dto/CreateAccountRequest.java
      dto/AccountResponse.java
      enums/AccountType.java
      enums/AccountStatus.java
    src/test/java/com/bank/account/
      service/AccountServiceTest.java
      controller/AccountControllerTest.java
  transaction-service/
    src/main/java/com/bank/transaction/
      entity/Transaction.java
      repository/TransactionRepository.java
      service/TransactionService.java
      controller/TransactionController.java
      dto/TransactionRequest.java
      dto/TransferRequest.java
      enums/TransactionType.java
      enums/TransactionStatus.java
    src/test/java/com/bank/transaction/
      service/TransactionServiceTest.java
  customer-service/
    src/main/java/com/bank/customer/
      entity/Customer.java
      repository/CustomerRepository.java
      service/CustomerService.java
      controller/CustomerController.java
      dto/CustomerRequest.java
      enums/CustomerType.java
      enums/RiskRating.java
    src/test/java/com/bank/customer/
      service/CustomerServiceTest.java
  settlement-batch/
    src/main/java/com/bank/settlement/
      config/SettlementBatchConfig.java
      processor/InterestCalculationProcessor.java
      processor/FeeAssessmentProcessor.java
      tasklet/ResetDailyCountersTasklet.java
      tasklet/SettlementReportTasklet.java
  audit-service/
    src/main/java/com/bank/audit/
      entity/AuditLog.java
      repository/AuditLogRepository.java
      service/AuditService.java
      service/CtrReportGenerator.java
      service/SarReportGenerator.java
      service/StructuringDetector.java
      consumer/TransactionAuditConsumer.java
  common/
    src/main/java/com/bank/common/
      exception/AccountNotFoundException.java
      exception/InsufficientFundsException.java
      exception/AccountFrozenException.java
      exception/DailyLimitExceededException.java
```

---

## 7. Success Criteria

- [ ] All 5 COBOL programs fully converted to Java with 100% business logic coverage
- [ ] Zero precision loss in financial calculations (validated by parallel run)
- [ ] All CTR/SAR reports match legacy system output exactly
- [ ] Online transaction latency <100ms p99
- [ ] Settlement batch completes within 30-minute window
- [ ] 90%+ unit test coverage on service classes
- [ ] Successful 1-week parallel run with zero reconciliation breaks
- [ ] Regulatory sign-off on audit trail completeness
