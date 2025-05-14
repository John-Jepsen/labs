Scaling with Load Balancers & Caching

## 📚 Concepts to Learn

- Load Balancing Algorithms (Round Robin, Least Connections, IP Hash)
- Horizontal Scaling vs. Vertical Scaling
- Caching Strategies (Cache-Aside, Read-Through, Write-Through, Write-Back)
- Cache Eviction Policies (LRU, LFU, FIFO)
- Content Delivery Network (CDN) Basics and Benefits
- Stateless vs. Stateful Application Design for Scalability

## 📊 Required Data Structures

You must identify and represent these in your diagrams:

- **LRU (Least Recently Used) Cache:** For managing cache entries efficiently.
- **Hash Maps/Dictionaries:** For storing short URL to long URL mappings, and for implementing cache lookups.
- **Distributed Hash Tables (Conceptual):** For understanding how a distributed cache might work (though not necessarily implementing the DHT itself).

## Whiteboarding Challenge

### Scenario

Design a highly available and scalable **URL Shortener service** (like bit.ly or tinyurl.com). The service must handle millions of daily requests for both creating short URLs and redirecting users from short URLs to their original long URLs. An efficient caching strategy is critical to minimize database load and reduce latency.

### Required Diagrams

For this lab, you will create the following three diagrams for the URL Shortener service:

1.  **Sequence Diagram (Short URL Resolution & Creation):**
    - **Action:** Create two UML Sequence Diagrams (or combine if exceptionally clear).
    - **Diagram 1 (Resolution):** Illustrate the complete lifecycle of a client request to resolve an existing short URL.
      - Participants: `Client`, `Load Balancer`, `Application Server`, `Cache Layer` (e.g., Redis), `Database`.
      - Show interactions for both cache hit and cache miss scenarios. This must clearly depict the `Client -> Load Balancer -> Application Server -> Cache Layer` check, then `Cache Layer -> Database` (on miss), data returned to `Application Server`, `Application Server -> Cache Layer` (to store on miss), and finally response to `Client`.
    - **Diagram 2 (Creation):** Separately, illustrate the flow for a client request to create a new short URL.
      - Participants: `Client`, `Load Balancer`, `Application Server`, `ShortURLGenerationLogic`, `Database`, `Cache Layer`.
      - Show how the long URL is received, a short URL is generated (conceptually), uniqueness is ensured (e.g., check against DB), the mapping is stored in the `Database` and potentially primed in the `Cache Layer`, and the short URL is returned to the `Client`.
2.  **Component Diagram (Overall System Architecture):**
    - **Action:** Create a comprehensive component diagram.
    - **Content:** Detail the full system architecture. You must include and show interactions between:
      - `DNS`
      - `CDN` (specify its role, e.g., static assets, cached redirects)
      - `Load Balancers` (indicate if L4/L7 and their placement)
      - A cluster of stateless `Application Servers`
      - `Distributed Cache Layer` (name a technology, e.g., Redis Cluster)
      - `Database` (indicate primary and any read replicas)
3.  **Data Flow Diagram (DFD - URL Operations):**
    - **Action:** Create a Level 0 or Level 1 Data Flow Diagram.
    - **Content:** Focus on the primary data flows for the two core operations:
      - **Short URL Creation:** Show `Long URL` as input, the system processing it, `Short URL` as output, and data being stored.
      - **Short URL Resolution:** Show `Short URL` as input, the system retrieving the mapping, and `Long URL` (or redirect) as output.
      - Clearly identify and label the key data stores involved, such as `URLMappingCache` and `MainURLDatabase`.

### Critical Architectural Decisions (Visualize and Justify)

1.  **Cache Placement and Strategy:**
    - **Visualization:** Diagram where the cache sits (e.g., in-memory on app servers, separate distributed cache cluster). Show the chosen caching strategy (e.g., Cache-Aside) with a mini-flowchart for read/write operations.
    - **Justification:** Explain why the chosen cache placement (e.g., distributed cache for shared access and independent scaling) and strategy (e.g., Cache-Aside for flexibility) are optimal for this use case, considering consistency, availability, and performance.
2.  **Database Schema and Short URL Generation:**
    - **Visualization:** A simple ERD for the database (e.g., a table mapping `short_code` to `long_url`, `created_at`, `user_id`). Illustrate the algorithm for generating unique short codes (e.g., base62 encoding of a counter, or hashing + collision resolution).
    - **Justification:** Defend the schema design for efficiency and scalability. Explain the chosen short URL generation technique, addressing potential collisions, uniqueness, and length of the short code.
3.  **Load Balancing Approach:**
    - **Visualization:** Show the load balancer(s) distributing traffic to multiple application server instances. Indicate the chosen load balancing algorithm (e.g., Round Robin, Least Connections) with a simple graphic.
    - **Justification:** Explain why horizontal scaling with load balancers is necessary. Justify the choice of load balancing algorithm based on the application's stateless nature and desired traffic distribution.

## ⚖️ Trade-off Discussion Points

1.  **SQL vs. NoSQL for URL Storage:**

    - **Visual Analysis:**
      - **Pros/Cons Table:**
        | Feature | SQL (e.g., PostgreSQL, MySQL) | NoSQL (e.g., Cassandra, DynamoDB - Key-Value) |
        |---------------------|--------------------------------------|-----------------------------------------------|
        | Schema | Predefined, structured schema | Flexible, schema-less or schema-on-read |
        | Scalability (Write) | Harder to scale writes (master-slave)| Easier to scale writes (distributed) |
        | Scalability (Read) | Read replicas help | Highly scalable reads |
        | Consistency | Strong consistency (ACID) | Tunable consistency (Eventual common) |
        | Querying | Powerful SQL, JOINs | Simpler get/put, limited querying |
        | Data Model | Relational | Key-Value, Document, Column-family etc. |
        | Use Case Fit | Good for complex relations, ACID needs | Good for high throughput, simple lookups |
      - **Decision Tree:** Factors to consider when choosing.
        ```mermaid
        graph TD
            A{Need for URL Shortener DB} --> B{High Write/Read Throughput?};
            B -- Yes --> C{Simple Key-Value Access Pattern? (short_url -> long_url)};
            C -- Yes --> D[Consider NoSQL (Key-Value)];
            C -- No --> E{Need for ACID Transactions / Complex Queries?};
            B -- No --> E;
            E -- Yes --> F[Consider SQL];
            E -- No --> G[Evaluate further based on specific NoSQL type];
            D --> H[Evaluate specific NoSQL options];
            F --> I[Evaluate specific SQL options];
        ```
    - **Discussion:** Analyze the requirements of a URL shortener (high read/write volume, simple key-value lookup) and debate whether a traditional SQL database or a NoSQL solution would be more appropriate. Consider factors like data consistency, scalability, and operational complexity.

2.  **Cache Invalidation Strategies vs. Time-To-Live (TTL):**

    - **Visual Analysis:**

      - **Pros/Cons Table:**
        | Strategy | Pros | Cons |
        |----------------------|-----------------------------------------------|-----------------------------------------------------------|
        | TTL Only | Simple to implement, eventual consistency | Stale data can exist until TTL expires |
        | Write-Through + TTL | Data in cache & DB always consistent | Higher write latency (write to cache + DB) |
        | Cache-Aside + Explicit Invalidation | Control over freshness, less stale data | More complex logic, potential for missed invalidations |
        | Write-Back | Very low write latency | Data loss risk if cache fails before write-back, complex |
      - **Flow Diagram:** Illustrate how an update to a long URL (if allowed) would trigger an explicit cache invalidation versus relying solely on TTL.

        ```mermaid
        sequenceDiagram
            participant UserApp as "Application Logic"
            participant Cache
            participant Database

            UserApp->>Database: Update long_url for short_code_X
            alt Explicit Invalidation
                UserApp->>Cache: Invalidate cache for short_code_X
            else Rely on TTL
                Note over Cache: short_code_X will expire after TTL
            end
        ```

    - **Discussion:** Discuss the implications of different cache invalidation methods (or lack thereof, relying only on TTL). When is stale data acceptable? What are the complexities and risks of implementing active cache invalidation for a URL shortener (where URLs typically don't change after creation, but this could be a hypothetical extension).

3.  **CDN Placement and Content:**
    - **Visual Analysis:**
      - **Diagram:** Show CDN edge locations serving requests geographically closer to users, reducing latency for certain content.
      - **Pros/Cons Table for CDN Usage:**
        | Aspect | Using CDN (Pros) | Using CDN (Cons) |
        |----------------|---------------------------------------------------|-------------------------------------------------------|
        | Latency | Reduced latency for users (geographical caching) | Potential for slightly stale data if not updated quickly|
        | Server Load | Reduced load on origin servers | Cost associated with CDN service |
        | Availability | Increased availability (content served from edge) | Complexity in cache invalidation across CDN |
        | Content Types | Best for static assets, can cache API responses | Less effective for highly dynamic, unique content |
    - **Discussion:** What content can a URL shortener serve via CDN? (e.g., static parts of its own website, potentially even very frequently accessed short URL redirects if the CDN supports dynamic content caching or edge compute). What are the trade-offs in terms of cost, complexity, and cache coherence?

## 📝 Gandalf Notes

### Common Design Pitfalls:

- **Single point of failure for cache:** Not considering a distributed cache.
- **Inefficient short URL generation:** Leading to collisions or overly long short URLs.
- **Ignoring cache eviction:** Assuming infinite cache memory.
- **Statelessness violation:** Storing session state in application servers, hindering horizontal scaling.
- **Database as the bottleneck:** Not effectively using caching to shield the database.
- **Underestimating traffic:** Designing a system that can't handle peak loads.

### Ideal Visual Solutions:

- **Sequence Diagram:** Clearly show the cache check before DB lookup. Highlight paths for cache hit and cache miss.
- **Component Diagram:** Load balancer prominently featured, multiple app server instances, a distinct cache cluster, and the database (possibly with read replicas shown).
- **DFD:** Simple, high-level flow showing where the URL mapping data originates, is stored, and is retrieved.

### Key Discussion Points:

- The importance of stateless application servers for easy horizontal scaling.
- How caching reduces latency and database load significantly.
- Different types of load balancers (L4 vs. L7) and when to use them.
- The CAP theorem in the context of choosing a database and cache (Availability vs. Consistency).
- Strategies for ensuring uniqueness in short URL generation at scale.
- Monitoring and alerting for a scaled system (cache hit/miss rates, server utilization, DB performance).

### Example Diagrams:

**1. Sequence Diagram (Short URL Resolution - Mermaid):**

```mermaid
sequenceDiagram
    participant Client
    participant LB as "Load Balancer"
    participant AppServer as "Application Server"
    participant CacheLayer as "Cache (e.g., Redis)"
    participant DB as "Database"

    Client->>+LB: GET /shortcode
    LB->>+AppServer: Forward GET /shortcode
    AppServer->>+CacheLayer: Get(shortcode)
    alt Cache Hit
        CacheLayer-->>-AppServer: Long URL
        AppServer-->>-LB: HTTP 301 Redirect (Long URL)
        LB-->>-Client: HTTP 301 Redirect (Long URL)
    else Cache Miss
        CacheLayer-->>-AppServer: Not Found
        AppServer->>+DB: FindByShortcode(shortcode)
        DB-->>-AppServer: Long URL Record
        AppServer->>+CacheLayer: Set(shortcode, Long URL, TTL)
        CacheLayer-->>-AppServer: OK
        AppServer-->>-LB: HTTP 301 Redirect (Long URL)
        LB-->>-Client: HTTP 301 Redirect (Long URL)
    end
```

**2. Component Diagram (Mermaid):**

```mermaid
graph TD
    subgraph "Internet"
        User[User Browser/Client]
    end

    subgraph "CDN Network"
        CDN[CDN Edge Locations]
    end

    subgraph "Our Datacenter/Cloud Region"
        LB[Load Balancer (L4/L7)]
        subgraph "Application Tier"
            App1[App Server 1]
            App2[App Server 2]
            AppN[App Server N ...]
        end
        subgraph "Caching Tier"
            CacheCluster[Distributed Cache Cluster (e.g., Redis)]
        end
        subgraph "Database Tier"
            DBMaster[Primary Database]
            DBReplica[Read Replica]
        end
    end

    User --> DNS
    DNS --> CDN
    DNS --> LB
    CDN --> LB # For dynamic content or API calls not cached by CDN
    CDN --> User # For static assets

    LB --> App1
    LB --> App2
    LB --> AppN

    App1 --> CacheCluster
    App2 --> CacheCluster
    AppN --> CacheCluster

    App1 --> DBMaster # Writes
    App2 --> DBMaster # Writes
    AppN --> DBMaster # Writes

    App1 --> DBReplica # Reads
    App2 --> DBReplica # Reads
    AppN --> DBReplica # Reads

    CacheCluster --> DBMaster # On cache miss, app server writes to DB, then cache
    DBMaster -.-> DBReplica # Replication
```

**3. Data Flow Diagram (DFD - Level 0 Simplified - Mermaid):**

```mermaid
graph TD
    A[User] -- Long URL for Shortening --> B(URL Shortener System);
    B -- Short URL --> A;
    A -- Short URL for Resolution --> B;
    B -- Redirect to Long URL --> A;

    subgraph URL Shortener System
        C[API Layer / App Servers]
        D[Cache]
        E[Database]
    end

    C -- Write/Read --> D;
    C -- Write/Read --> E;
    D -.-> C # Cache Hit
    E -.-> C # DB Read
```

### Recommended Tools:

- Physical Whiteboards + Markers
- Digital Whiteboarding: Miro, Lucidspark
- Diagramming Tools: Lucidchart, draw.io (diagrams.net)
- Mermaid.js for markdown-based diagrams.

---

**End of Lab 2**
