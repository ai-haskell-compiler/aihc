You are tasked with identifying and fixing the root cause of a known test failure (currently marked xfail), and implementing a robust, long-term solution.

1. Root Cause Analysis
   - Reproduce and clearly describe the failure.
   - Identify the exact root cause (not just symptoms).
   - Explain why the current implementation allows this failure.
   - Identify any underlying design or architectural weaknesses.

2. Solution Design
   - Propose 2–3 possible solutions.
   - Evaluate tradeoffs (correctness, maintainability, complexity, performance).
   - Select the best long-term solution and justify your choice.
   - Ensure the solution generalizes beyond the specific failing test.

3. Implementation
   - Implement the chosen solution with clean, maintainable code.
   - Follow existing project conventions and patterns.
   - Avoid unnecessary complexity or over-engineering.
   - Ensure no regressions in existing functionality.

4. Testing
   - Add or update tests to:
     - Cover the original failure
     - Validate the fix
     - Include relevant edge cases
   - Ensure the test suite clearly demonstrates the issue is resolved.

5. Refactoring Opportunities
   - Identify any additional refactoring opportunities discovered.
   - For non-essential changes, create separate GitHub issues.
   - Clearly explain why they are not included in this PR.

6. Pull Request
   - Open a PR that includes:
     - A clear explanation of the root cause
     - Description of the chosen solution and reasoning
     - Summary of changes made
     - Notes on any follow-up work
