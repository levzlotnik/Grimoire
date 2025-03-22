from itertools import chain
from pyswip import Prolog as p
from pyswip.prolog import PrologError
from typing import List, Dict, Any, Optional, Tuple
from pydantic import BaseModel, Field
import os

from ..common import wrap_tool_error


class QueryResult(BaseModel):
    """Result of executing a Prolog query"""

    yes_no_result: Optional[bool] = Field(
        None, description="The result of a yes/no query"
    )

    variables: Optional[List[Dict[str, Any]]] = Field(
        None,
        description="The variables' values returned by the query, as a list of dictionaries",
    )


class ApplyActionResult(BaseModel):
    """Result of executing a system action"""

    success: bool = Field(..., description="Whether the action was successful")
    error: Optional[str] = Field(None, description="Error message if the action failed")


class PrologRule(BaseModel):
    """Explanation of a rule"""

    rule: str = Field(..., description="The rule name")
    explanation: str = Field(..., description="The explanation of the rule")


class GetAllRulesResult(BaseModel):
    """Result of getting all rules"""

    rules: List[PrologRule] = Field(..., description="All rules")


class MakeRuleResult(BaseModel):
    """Result of creating a new rule"""

    success: bool = Field(..., description="Whether the rule was created successfully")
    error: Optional[str] = Field(None, description="Error message if creation failed")


class GetObjectResult(BaseModel):
    """Result of getting object information"""

    exists: bool = Field(..., description="Whether the entity exists")
    components: Optional[Dict[str, Any]] = Field(
        None, description="Component values for the entity"
    )


class PrologToolset:
    """Engine for executing actions and rules in the semantic layer"""

    def __init__(self):
        # Load core rules
        rules_dir = os.path.dirname(__file__)
        p.consult(os.path.join(rules_dir, "core_rules.pl"))

    @wrap_tool_error(exc_types=(PrologError,))
    def query(self, query: str) -> QueryResult:
        """Execute a Prolog query and return results.
        To use a functor, it must be an existing predicate in the Prolog knowledge base.
        Any variables in the query must be capitalized as with normal Prolog.

        Returns:
            QueryResult with a list of results,
            each of which is a dictionary of variable names to values.
        """
        results = p.query(query)
        if results is None:
            return QueryResult(yes_no_result=False)
        elif results == {}:
            return QueryResult(yes_no_result=True)
        else:
            return QueryResult(variables=list(results))

    @wrap_tool_error(exc_types=(PrologError,))
    def apply_action(self, action: str) -> ApplyActionResult:
        """
        Execute an action with validation.

        Returns:
            ApplyActionResult with success status and optional error message
        """
        results = p.query(f"check_action({action}, Results)")
        if results is None or results == {}:
            return ApplyActionResult(success=False, error="Invalid action format")
        results = list(results)

        validation_results = results[0]["Results"]
        if validation_results == ["ok"]:
            p.query(f"commit({action})")
            return ApplyActionResult(success=True)
        return ApplyActionResult(
            success=False, error="; ".join(str(error) for error in validation_results)
        )

    @wrap_tool_error(exc_types=(PrologError,))
    def declare_predicate(
        self, predicate: str, args: List[str], docstring: str
    ) -> None:
        """Declare a predicate without adding it to the knowledge base.
        In Prolog, this is done by querying `:- dynamic(Predicate/Arity)`.

        Args:
            predicate: The predicate to declare
            args: The arguments of the predicate
            docstring: The docstring of the predicate
        """
        arity = len(args)
        p.dynamic(f"{predicate}/{arity}")
        p.assertz(f"docstring('{predicate}', '{docstring}')")

    @wrap_tool_error(exc_types=(PrologError,))
    def make_rule(
        self,
        rule: str,
        explanation: str,
        is_llm_rule: bool = False,
        context: Dict[str, Any] = None,
    ) -> MakeRuleResult:
        """
        Add a new rule.
        For LLM rules, context provides additional information for interpretation.
        For regular rules, do not include `.` at the end of the rule - it will be added automatically.

        Args:
            rule: The rule to add
            explanation: The explanation of the rule
            is_llm_rule: Whether the rule is an LLM rule
            context: Additional context for LLM rules

        Returns:
            MakeRuleResult with success status and optional error message
        """
        try:
            if is_llm_rule:
                context_str = str(context) if context else "{}"
                p.assertz(f"llm_rule({rule}, {context_str})")
            else:
                if rule.endswith("."):
                    rule = rule[:-1]
                p.assertz(rule)
            # Store rule as string
            p.assertz(f"docstring('{rule}', '{explanation}')")
            return MakeRuleResult(success=True)
        except Exception as e:
            print(f"Failed to create rule: {e}")
            return MakeRuleResult(success=False, error=str(e))

    @wrap_tool_error(exc_types=(PrologError,))
    def get_all_rules(self) -> GetAllRulesResult:
        """
        Get all rules and their documentation.

        Returns:
            List of RuleExplanation dictionaries
        """
        # Get regular rules with docstrings
        rules = p.query("docstring(Rule, Explanation)")

        # Get LLM rules with context
        llm_rules = p.query("llm_rule(Rule, Context), docstring(Rule, Explanation)")

        all_rules = [
            PrologRule(rule=d["Rule"], explanation=d["Explanation"])
            for d in chain(rules, llm_rules)
        ]

        return GetAllRulesResult(rules=all_rules)

    @wrap_tool_error(exc_types=(PrologError,))
    def get_object(self, entity_id: str) -> GetObjectResult:
        """
        Get complete information about an entity.

        Args:
            entity_id: The ID of the entity to get information about

        Returns:
            Nested dictionary of components and values
        """
        results = p.query(f"get_components({entity_id}, Components)")
        if not results:
            return GetObjectResult(components={}, exists=False)
        components = results[0]["Components"]
        return GetObjectResult(
            components={comp: value for comp, value in components}, exists=True
        )


if __name__ == "__main__":
    prolog = PrologToolset()

    # Example: Create and query an entity
    prolog.apply_action("create_entity(chest1)")
    prolog.apply_action("set_component(chest1, type, container)")
    prolog.apply_action("set_component(chest1, contents, [gold, potion])")

    # Get object info
    print(prolog.get_object("chest1"))

    # Make a new rule
    prolog.make_rule(
        "can_open(Actor, Container) :- has_component(Actor, type, character), has_component(Container, type, container)",
        "Characters can open containers",
    )

    # List all rules
    print(prolog.get_all_rules())
