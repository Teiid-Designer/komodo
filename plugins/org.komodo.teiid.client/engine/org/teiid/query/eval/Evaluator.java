/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

package org.teiid.query.eval;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.transform.stream.StreamResult;

import net.sf.saxon.om.NodeInfo;
import net.sf.saxon.query.QueryResult;
import net.sf.saxon.trans.XPathException;

import org.komodo.spi.query.sql.lang.CompareCriteria;
import org.komodo.spi.query.sql.lang.MatchCriteria.MatchMode;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.udf.FunctionLibrary;
import org.teiid.core.types.ArrayImpl;
import org.teiid.core.types.BaseLob;
import org.teiid.core.types.BlobType;
import org.teiid.core.types.ClobType;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.core.types.InputStreamFactory;
import org.teiid.core.types.SQLXMLImpl;
import org.teiid.core.types.Sequencable;
import org.teiid.core.types.Streamable;
import org.teiid.core.types.XMLType;
import org.teiid.core.types.XMLType.Type;
import org.teiid.core.types.basic.StringToSQLXMLTransform;
import org.teiid.core.util.StringUtil;
import org.teiid.language.SQLConstants;
import org.teiid.metadata.FunctionMethod.PushDown;
import org.teiid.query.function.TCFunctionDescriptor;
import org.teiid.query.function.JSONFunctionMethods.JSONBuilder;
import org.teiid.query.function.source.XMLSystemFunctions;
import org.teiid.query.function.source.XMLSystemFunctions.XmlConcat;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.query.sql.lang.AbstractCompareCriteria;
import org.teiid.query.sql.lang.AbstractSetCriteria;
import org.teiid.query.sql.lang.CollectionValueIteratorImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CompoundCriteriaImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.ExpressionCriteriaImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.NamespaceItem;
import org.teiid.query.sql.lang.NotCriteriaImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl.PredicateQuantifier;
import org.teiid.query.sql.lang.BaseSubqueryContainer;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.proc.ExceptionExpressionImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.ArraySymbolImpl;
import org.teiid.query.sql.symbol.CaseExpressionImpl;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.DerivedColumnImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.JSONObjectImpl;
import org.teiid.query.sql.symbol.QueryStringImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;
import org.teiid.query.sql.symbol.SearchedCaseExpressionImpl;
import org.teiid.query.sql.symbol.TextLineImpl;
import org.teiid.query.sql.symbol.BaseWindowFunction;
import org.teiid.query.sql.symbol.XMLElementImpl;
import org.teiid.query.sql.symbol.XMLForestImpl;
import org.teiid.query.sql.symbol.XMLNamespacesImpl;
import org.teiid.query.sql.symbol.XMLParseImpl;
import org.teiid.query.sql.symbol.XMLQueryImpl;
import org.teiid.query.sql.symbol.XMLSerializeImpl;
import org.teiid.query.sql.util.ValueIterator;
import org.teiid.query.sql.visitor.ValueIteratorProviderCollectorVisitorImpl;
import org.teiid.query.util.CommandContext;
import org.teiid.query.xquery.saxon.SaxonXQueryExpression;
import org.teiid.query.xquery.saxon.SaxonXQueryExpression.Result;
import org.teiid.query.xquery.saxon.SaxonXQueryExpression.RowProcessor;
import org.teiid.query.xquery.saxon.XQueryEvaluator;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.TeiidClientException;
import org.teiid.translator.SourceSystemFunctions;

/**
 *
 */
public class Evaluator {

    private final class XMLQueryRowProcessor implements RowProcessor {
		XmlConcat concat; //just used to get a writer
		Type type;
		private javax.xml.transform.Result result;
		
		private XMLQueryRowProcessor() throws Exception {
			concat = new XmlConcat();
			result = new StreamResult(concat.getWriter());
		}

		@Override
		public void processRow(NodeInfo row) {
			if (type == null) {
				type = SaxonXQueryExpression.getType(row);
			} else {
				type = Type.CONTENT;
			}
			try {
				QueryResult.serialize(row, result, SaxonXQueryExpression.DEFAULT_OUTPUT_PROPERTIES);
			} catch (XPathException e) {
				 throw new RuntimeException(e);
			}
		}
	}

	private final class SequenceReader extends Reader {
		private LinkedList<Reader> readers;
		private Reader current = null;
		
		public SequenceReader(LinkedList<Reader> readers) {
			this.readers = readers;
		}

		@Override
		public void close() {
			for (Reader reader : readers) {
				try {
					reader.close();
				} catch (IOException e) {
					
				}
			}
		}

		@Override
		public int read(char[] cbuf, int off, int len)
				throws IOException {
			if (current == null && !readers.isEmpty()) {
				current = readers.removeFirst();
			}
			if (current == null) {
				return -1;
			}
			int read = current.read(cbuf, off, len);
			if (read == -1) {
				current.close();
				current = null;
				read = 0;
			} 
			if (read < len) {
				int nextRead = read(cbuf, off + read, len - read);
				if (nextRead > 0) {
					read += nextRead;
				}
			}
			return read;
		}
	}

	@SuppressWarnings( "javadoc" )
	public static class NameValuePair<T> {
        public String name;
		public T value;
		
		public NameValuePair(String name, T value) {
			this.name = name;
			this.value = value;
		}
	}

	/**
	 *
	 * @param <T>
	 */
	public static interface ValueExtractor<T> {
        /**
         * @param t
         * @return extracted value
         */
        Object getValue(T t);
    }

	private final static char[] REGEX_RESERVED = new char[] {'$', '(', ')', '*', '+', '.', '?', '[', '\\', ']', '^', '{', '|', '}'}; //in sorted order
    private final static MatchCriteriaImpl.PatternTranslator LIKE_TO_REGEX = new MatchCriteriaImpl.PatternTranslator(new char[] {'%', '_'}, new String[] {".*", "."},  REGEX_RESERVED, '\\', Pattern.DOTALL);  //$NON-NLS-1$ //$NON-NLS-2$
    
    private final static char[] SIMILAR_REGEX_RESERVED = new char[] {'$', '.', '\\', '^'}; //in sorted order
    private final static MatchCriteriaImpl.PatternTranslator SIMILAR_TO_REGEX = new MatchCriteriaImpl.PatternTranslator(
    		new char[] {'%', '(', ')', '*', '?', '+', '[', ']', '_', '{', '|', '}'}, 
    		new String[] {"([a]|[^a])*", "(", ")", "*", "?", "+", //$NON-NLS-1$ //$NON-NLS-2$  //$NON-NLS-3$ //$NON-NLS-4$  //$NON-NLS-5$ //$NON-NLS-6$
    				"[", "]", "([a]|[^a])", "{", "|", "}"},  SIMILAR_REGEX_RESERVED, '\\', 0);  //$NON-NLS-1$ //$NON-NLS-2$  //$NON-NLS-3$ //$NON-NLS-4$  //$NON-NLS-5$ //$NON-NLS-6$  

    private final CommandContext commandContext;

    /**
     * @param teiidVersion
     */
    public Evaluator(TeiidVersion teiidVersion) {
        commandContext = new CommandContext(teiidVersion);
    }

    /**
     * @param criteria
     * @return evaluation of criteria
     * @throws Exception
     */
    public static boolean assess(CriteriaImpl criteria) throws Exception {
    	return new Evaluator(criteria.getTeiidVersion()).evaluate(criteria);
    }
    
    /**
     * @param expression
     * @return evaluation of expression
     * @throws Exception
     */
    public static Object assess(BaseExpression expression) throws Exception {
    	return new Evaluator(expression.getTeiidVersion()).evaluate(expression);
    }

	/**
	 * @param criteria
	 * @return evaluation of criteria
	 * @throws Exception
	 */
	public boolean evaluate(CriteriaImpl criteria)
        throws Exception {

        return Boolean.TRUE.equals(evaluateTVL(criteria));
    }

    private Boolean evaluateTVL(CriteriaImpl criteria)
        throws Exception {
    	
		return internalEvaluateTVL(criteria);
	}

	private Boolean internalEvaluateTVL(CriteriaImpl criteria)
			throws Exception {
		if(criteria instanceof CompoundCriteriaImpl) {
			return evaluate((CompoundCriteriaImpl)criteria);
		} else if(criteria instanceof NotCriteriaImpl) {
			return evaluate((NotCriteriaImpl)criteria);
		} else if(criteria instanceof CompareCriteriaImpl) {
			return evaluate((CompareCriteriaImpl)criteria);
		} else if(criteria instanceof MatchCriteriaImpl) {
			return evaluate((MatchCriteriaImpl)criteria);
		} else if(criteria instanceof AbstractSetCriteria) {
			return evaluate((AbstractSetCriteria)criteria);
		} else if(criteria instanceof IsNullCriteriaImpl) {
			return Boolean.valueOf(evaluate((IsNullCriteriaImpl)criteria));
        } else if(criteria instanceof SubqueryCompareCriteriaImpl) {
            return evaluate((SubqueryCompareCriteriaImpl)criteria);
        } else if(criteria instanceof ExistsCriteriaImpl) {
            return Boolean.valueOf(evaluate((ExistsCriteriaImpl)criteria));
        } else if (criteria instanceof ExpressionCriteriaImpl) {
        	return (Boolean)evaluate(((ExpressionCriteriaImpl)criteria).getExpression());
		} else {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30311, criteria));
		}
	}

	private Boolean evaluate(CompoundCriteriaImpl criteria)
		throws Exception {

		List<CriteriaImpl> subCrits = criteria.getCriteria();
		boolean and = criteria.getOperator() == CompoundCriteriaImpl.AND;
        Boolean result = and?Boolean.TRUE:Boolean.FALSE;
		for (int i = 0; i < subCrits.size(); i++) {
			CriteriaImpl subCrit = subCrits.get(i);
			Boolean value = internalEvaluateTVL(subCrit);
            if (value == null) {
				result = null;
			} else if (!value.booleanValue()) {
				if (and) {
					return Boolean.FALSE;
				}
            } else if (!and) {
            	return Boolean.TRUE;
            }
		}
		return result;
	}

	private Boolean evaluate(NotCriteriaImpl criteria)
		throws Exception {

		CriteriaImpl subCrit = criteria.getCriteria();
		Boolean result = internalEvaluateTVL(subCrit);
        if (result == null) {
            return null;
        }
        if (result.booleanValue()) {
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
	}

	private Boolean evaluate(CompareCriteriaImpl criteria)
		throws Exception {

		// Evaluate left expression
		Object leftValue = null;
		try {
			leftValue = evaluate(criteria.getLeftExpression());
		} catch(Exception e) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30312, "left", criteria)); //$NON-NLS-1$
		}

		// Shortcut if null
		if(leftValue == null) {
			return null;
		}

		// Evaluate right expression
		Object rightValue = null;
		try {
			rightValue = evaluate(criteria.getRightExpression());
		} catch(Exception e) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30312, "right", criteria)); //$NON-NLS-1$
		}

		// Shortcut if null
		if(rightValue == null) {
			return null;
		}

		// Compare two non-null values using specified operator
		return compare(criteria, leftValue, rightValue);
	}

	private Boolean evaluate(MatchCriteriaImpl criteria)
		throws Exception {

        boolean result = false;
		// Evaluate left expression
        Object value = null;
		try {
			value = evaluate(criteria.getLeftExpression());
		} catch(Exception e) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30312, "left", criteria)); //$NON-NLS-1$
		}

		// Shortcut if null
		if(value == null) {
            return null;
        }
        
        CharSequence leftValue = null;
        
        if (value instanceof CharSequence) {
            leftValue = (CharSequence)value;
        } else {
            try {
                leftValue = ((Sequencable)value).getCharSequence();
            } catch (SQLException err) {
                 throw new TeiidClientException(err, err.getMessage());
            }
        }

		// Evaluate right expression
		String rightValue = null;
		try {
			rightValue = (String) evaluate(criteria.getRightExpression());
		} catch(Exception e) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30312, "right", criteria)); //$NON-NLS-1$
		}

		// Shortcut if null
		if(rightValue == null) {
            return null;
        }
        
        result = match(rightValue, criteria.getEscapeChar(), leftValue, criteria.getMode());
        
        return Boolean.valueOf(result ^ criteria.isNegated());
	}

	private boolean match(String pattern, char escape, CharSequence search, MatchMode mode)
		throws Exception {

		Pattern patternRegex = null;
		switch (mode) {
		case LIKE:
			patternRegex = LIKE_TO_REGEX.translate(pattern, escape);
			break;
		case SIMILAR:
			patternRegex = SIMILAR_TO_REGEX.translate(pattern, escape);
			break;
		case REGEX:
			patternRegex = MatchCriteriaImpl.getPattern(pattern, pattern, 0);
			break;
		default:
			throw new AssertionError();
		}
		
        Matcher matcher = patternRegex.matcher(search);
        return matcher.find();
	}

	private Boolean evaluate(AbstractSetCriteria criteria)
		throws Exception {

	    TeiidClientParser teiidParser = criteria.getTeiidParser();

		// Evaluate expression
		Object leftValue = null;
		try {
			leftValue = evaluate(criteria.getExpression());
		} catch(Exception e) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30323, criteria));
		}

        Boolean result = Boolean.FALSE;

        ValueIterator valueIter = null;
        if (criteria instanceof SetCriteriaImpl) {
        	SetCriteriaImpl set = (SetCriteriaImpl)criteria;
    		// Shortcut if null
    		if(leftValue == null) {
    			if (!set.getValues().isEmpty()) {
    				return null;
    			}
    			return criteria.isNegated();
        	}
        	if (set.isAllConstants()) {
        	    ConstantImpl c = teiidParser.createASTNode(ASTNodes.CONSTANT);
        	    c.setValue(leftValue);
        	    c.setType(criteria.getExpression().getType());
        		boolean exists = set.getValues().contains(c);
        		if (!exists) {
        			if (set.getValues().contains(ConstantImpl.getNullConstant(teiidParser))) {
        				return null;
        			}
        			return criteria.isNegated();
        		}
        		return !criteria.isNegated();
        	}
        	valueIter = new CollectionValueIteratorImpl(((SetCriteriaImpl)criteria).getValues());
        } else if (criteria instanceof SubquerySetCriteriaImpl) {
        	try {
				valueIter = evaluateSubquery((SubquerySetCriteriaImpl)criteria);
			} catch (Exception e) {
				 throw new TeiidClientException(e);
			}
        } else {
        	throw new AssertionError("unknown set criteria type"); //$NON-NLS-1$
        }
        while(valueIter.hasNext()) {
        	if(leftValue == null) {
    			return null;
        	}
            Object possibleValue = valueIter.next();
            Object value = null;
            if(possibleValue instanceof BaseExpression) {
    			try {
    				value = evaluate((BaseExpression) possibleValue);
    			} catch(Exception e) {
                     throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30323, possibleValue));
    			}
            } else {
                value = possibleValue;
            }

			if(value != null) {
				if(ConstantImpl.COMPARATOR.compare(leftValue, value) == 0) {
					return Boolean.valueOf(!criteria.isNegated());
				} // else try next value
			} else {
			    result = null;
            }
		}
        
        if (result == null) {
            return null;
        }
        
        return Boolean.valueOf(criteria.isNegated());
	}

	private boolean evaluate(IsNullCriteriaImpl criteria)
		throws Exception {

		// Evaluate expression
		Object value = null;
		try {
			value = evaluate(criteria.getExpression());
		} catch(Exception e) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30323, criteria));
		}

		return (value == null ^ criteria.isNegated());
	}

    private Boolean evaluate(SubqueryCompareCriteriaImpl criteria)
        throws Exception {

        // Evaluate expression
        Object leftValue = null;
        try {
            leftValue = evaluate(criteria.getLeftExpression());
        } catch(Exception e) {
             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30323, criteria));
        }

        // Need to be careful to initialize this variable carefully for the case
        // where valueIterator has no values, and the block below is not entered.
        // If there are no rows, and ALL is the predicate quantifier, the result
        // should be true.  If SOME is the predicate quantifier, or no quantifier
        // is used, the result should be false.
        Boolean result = Boolean.FALSE;
        if (criteria.getPredicateQuantifier() == PredicateQuantifier.ALL){
            result = Boolean.TRUE;
        }

        ValueIterator valueIter;
		try {
			valueIter = evaluateSubquery(criteria);
		} catch (Exception e) {
			 throw new TeiidClientException(e);
		}
        while(valueIter.hasNext()) {
            Object value = valueIter.next();
            
            // Shortcut if null
            if(leftValue == null) {
                return null;
            }

            if(value != null) {
            	result = compare(criteria, leftValue, value);

                switch(criteria.getPredicateQuantifier()) {
                    case ALL:
                        if (Boolean.FALSE.equals(result)){
                            return Boolean.FALSE;
                        }
                        break;
                    case SOME:
                        if (Boolean.TRUE.equals(result)){
                            return Boolean.TRUE;
                        }
                        break;
                    default:
                         throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30326, criteria.getPredicateQuantifier()));
                }

            } else { // value is null
                result = null;
            }


        } //end value iteration

        return result;
    }

	/**
	 * @param criteria
	 * @param leftValue
	 * @param value
	 * @return true if left value matches the value according to the criteria
	 * @throws AssertionError
	 */
	public static Boolean compare(AbstractCompareCriteria criteria, Object leftValue,
			Object value) throws AssertionError {
		int compare = 0;
		//TODO: we follow oracle style array comparison
		//semantics.  each element is treated as an individual comparison,
		//so null implies unknown. h2 (and likely other dbms) allow for null
		//array element equality
		if (leftValue instanceof ArrayImpl) {
			ArrayImpl av = (ArrayImpl)leftValue;
			try {
				compare = av.compareTo((ArrayImpl)value, true, ConstantImpl.COMPARATOR);
			} catch (ArrayImpl.NullException e) {
				return null;
			}
		} else {
			compare = ConstantImpl.COMPARATOR.compare(leftValue, value);
		}
		// Compare two non-null values using specified operator
		Boolean result = null;
		switch(criteria.getOperator()) {
		    case CompareCriteria.EQ:
		        result = Boolean.valueOf(compare == 0);
		        break;
		    case CompareCriteria.NE:
		        result = Boolean.valueOf(compare != 0);
		        break;
		    case CompareCriteria.LT:
		        result = Boolean.valueOf(compare < 0);
		        break;
		    case CompareCriteria.LE:
		        result = Boolean.valueOf(compare <= 0);
		        break;
		    case CompareCriteria.GT:
		        result = Boolean.valueOf(compare > 0);
		        break;
		    case CompareCriteria.GE:
		        result = Boolean.valueOf(compare >= 0);
		        break;
		    default:
		        throw new AssertionError();
		}
		return result;
	}

    private boolean evaluate(ExistsCriteriaImpl criteria)
        throws Exception {

        ValueIterator valueIter;
		try {
			valueIter = evaluateSubquery(criteria);
		} catch (Exception e) {
			 throw new TeiidClientException(e);
		}
        if(valueIter.hasNext()) {
            return !criteria.isNegated();
        }
        return criteria.isNegated();
    }
    
	/**
	 * @param expression
	 * @return evaluated value of the given expression
	 * @throws Exception
	 */
	public Object evaluate(BaseExpression expression)
		throws Exception {
	
	    try {
			return internalEvaluate(expression);
	    } catch (Exception e) {
	         throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30328, new Object[] {expression, e.getMessage()}));
	    }
	}
	
	protected Object internalEvaluate(BaseExpression expression)
	   throws Exception {
	
	   if(expression instanceof BaseAggregateSymbol || expression instanceof AliasSymbolImpl ||
	       expression instanceof ElementSymbolImpl || expression instanceof BaseWindowFunction) {
	       throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30328, expression, Messages.getString(Messages.Misc.Evaluator_noValue)));
	   }
	   // ExpressionSymbol so we just need to dive in and evaluate the expression itself
       if (expression instanceof ExpressionSymbolImpl) {            
           ExpressionSymbolImpl exprSyb = (ExpressionSymbolImpl) expression;
           BaseExpression expr = exprSyb.getExpression();
           return internalEvaluate(expr);
       } 
	   if(expression instanceof ConstantImpl) {
	       return ((ConstantImpl) expression).getValue();
	   } else if(expression instanceof FunctionImpl) {
	       return evaluate((FunctionImpl) expression);
	   } else if(expression instanceof CaseExpressionImpl) {
	       return evaluate((CaseExpressionImpl) expression);
	   } else if(expression instanceof SearchedCaseExpressionImpl) {
	       return evaluate((SearchedCaseExpressionImpl) expression);
	   } else if(expression instanceof ReferenceImpl) {
		   ReferenceImpl ref = (ReferenceImpl)expression;
		   if (ref.isPositional() && ref.getExpression() == null) {
		       throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30328, expression, Messages.getString(Messages.Misc.Evaluator_noValue)));
		   }
		   Object result = internalEvaluate(ref.getExpression());
		   if (ref.getConstraint() != null) {
			   try {
				   ref.getConstraint().validate(result);
			   } catch (Exception e) {
				   throw new TeiidClientException(e);
			   }
		   }
		   return result;
	   } else if(expression instanceof CriteriaImpl) {
	       return evaluate((CriteriaImpl) expression);
	   } else if(expression instanceof ScalarSubqueryImpl) {
	       return evaluate((ScalarSubqueryImpl) expression);
	   } else if (expression instanceof CriteriaImpl) {
		   return evaluate((CriteriaImpl)expression);
	   } else if (expression instanceof TextLineImpl){
		   return evaluateTextLine((TextLineImpl)expression);
	   } else if (expression instanceof XMLElementImpl){
		   return evaluateXMLElement((XMLElementImpl)expression);
	   } else if (expression instanceof XMLForestImpl){
		   return evaluateXMLForest((XMLForestImpl)expression);
	   } else if (expression instanceof JSONObjectImpl){
		   return evaluateJSONObject((JSONObjectImpl)expression, null);
	   } else if (expression instanceof XMLSerializeImpl){
		   return evaluateXMLSerialize((XMLSerializeImpl)expression);
	   } else if (expression instanceof XMLQueryImpl) {
		   return evaluateXMLQuery((XMLQueryImpl)expression);
	   } else if (expression instanceof QueryStringImpl) {
		   return evaluateQueryString((QueryStringImpl)expression);
	   } else if (expression instanceof XMLParseImpl){
		   return evaluateXMLParse((XMLParseImpl)expression);
	   } else if (expression instanceof ArraySymbolImpl) {
		   ArraySymbolImpl array = (ArraySymbolImpl)expression;
		   List<BaseExpression> exprs = array.getExpressions();
		   Object[] result = (Object[]) java.lang.reflect.Array.newInstance(array.getComponentType(), exprs.size());
		   for (int i = 0; i < exprs.size(); i++) {
			   Object eval = internalEvaluate(exprs.get(i));
			   if (eval instanceof ArrayImpl) {
				   eval = ((ArrayImpl)eval).getValues();
			   }
			   result[i] = eval;
		   }
		   return new ArrayImpl(expression.getTeiidVersion(), result);
	   } else if (expression instanceof ExceptionExpressionImpl) {
		   return evaluate((ExceptionExpressionImpl)expression);
	   } else {
	        throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30329, expression.getClass().getName()));
	   }
	}

	@SuppressWarnings( "unused" )
    private Object evaluate(ExceptionExpressionImpl ee)
			throws Exception {
		String msg = (String) internalEvaluate(ee.getMessage());
		String sqlState = ee.getDefaultSQLState();
		if (ee.getSqlState() != null) {
			sqlState = (String) internalEvaluate(ee.getSqlState());
		}
		Integer errorCode = null;
		if (ee.getErrorCode() != null) {
			errorCode = (Integer) internalEvaluate(ee.getErrorCode());
		}
		Exception parent = null;
		if (ee.getParent() != null) {
			parent = (Exception) internalEvaluate(ee.getParent());
		}
		msg = errorCode != null ? errorCode + " : " + msg : msg; //$NON-NLS-1$
		Exception result = new TeiidClientException(parent, msg);
		return result;
	}

	private Object evaluateXMLParse(final XMLParseImpl xp) throws Exception {
		Object value = internalEvaluate(xp.getExpression());
		if (value == null) {
			return null;
		}
		XMLType.Type type = Type.DOCUMENT;
		SQLXMLImpl result = null;
		try {
			if (value instanceof String) {
				String string = (String)value;
				result = new SQLXMLImpl(string);
				result.setCharset(Streamable.CHARSET);
				if (!xp.isWellFormed()) {
					Reader r = new StringReader(string);
					type = validate(xp, r);
				}
			} else {
				InputStreamFactory isf = null;
				Streamable<?> s = (Streamable<?>)value;
				isf = getInputStreamFactory(s);
				result = new SQLXMLImpl(isf);
				if (!xp.isWellFormed()) {
					Reader r = result.getCharacterStream();
					type = validate(xp, r);
				}
			}
		} catch (Exception e) {
			 throw new TeiidClientException(e);
		}
		if (!xp.isDocument()) {
			type = Type.CONTENT;
		}
		XMLType xml = new XMLType(result);
		xml.setType(type);
		return xml;
	}

	/**
	 * @param s
	 * @return input stream factory
	 */
	public static InputStreamFactory getInputStreamFactory(Streamable<?> s) {
		if (s.getReference() instanceof Streamable<?>) {
			return getInputStreamFactory((Streamable<?>) s.getReference());
		}
		if (s.getReference() instanceof BaseLob) {
			BaseLob bl = (BaseLob) s.getReference();
			try {
				InputStreamFactory isf = bl.getStreamFactory();
				if (isf != null) {
					return isf;
				}
			} catch (SQLException e) {
			}
		}
		if (s instanceof ClobType) {
			return new InputStreamFactory.ClobInputStreamFactory((Clob)s.getReference());
		} else if (s instanceof BlobType){
			return new InputStreamFactory.BlobInputStreamFactory((Blob)s.getReference());
		}
		return new InputStreamFactory.SQLXMLInputStreamFactory((SQLXML)s.getReference());
	}

	private Type validate(final XMLParseImpl xp, Reader r)
			throws Exception {
		if (!xp.isDocument()) {
			LinkedList<Reader> readers = new LinkedList<Reader>();
			readers.add(new StringReader("<r>")); //$NON-NLS-1$
			readers.add(r);
			readers.add(new StringReader("</r>")); //$NON-NLS-1$
			r = new SequenceReader(readers);
		}
		return StringToSQLXMLTransform.isXml(r);
	}

	/**
	 * Taken from WSConnection
	 *
	 * @param s
	 * @return
	 */
	private String httpURLEncode(String s) {
        try {
            return URLEncoder.encode(s, "UTF-8").replaceAll("\\+", "%20"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

	//TODO: exception if length is too long?
	private Object evaluateQueryString(QueryStringImpl queryString)
			throws Exception {
		Evaluator.NameValuePair<Object>[] pairs = getNameValuePairs(queryString.getArgs(), false, true);
		String path = (String)internalEvaluate(queryString.getPath());
		if (path == null) {
			path = ""; //$NON-NLS-1$
		} 
		boolean appendedAny = false;
		StringBuilder result = new StringBuilder();
		for (Evaluator.NameValuePair<Object> nameValuePair : pairs) {
			if (nameValuePair.value == null) {
				continue;
			}
			if (appendedAny) {
				result.append('&');
			}
			appendedAny = true;
			result.append(httpURLEncode(nameValuePair.name)).append('=').append(httpURLEncode((String)nameValuePair.value));
		}
		if (!appendedAny) {
			return path;
		}
		result.insert(0, '?');
		result.insert(0, path);
		return result.toString();
	}

	private Object evaluateXMLQuery(XMLQueryImpl xmlQuery)
			throws Exception {
		boolean emptyOnEmpty = xmlQuery.getEmptyOnEmpty() == null || xmlQuery.getEmptyOnEmpty();
		Result result = null;
		try {
			XMLQueryRowProcessor rp = null;
			if (xmlQuery.getXQueryExpression().isStreaming()) {
				rp = new XMLQueryRowProcessor();
			}
			try {
				result = evaluateXQuery(xmlQuery.getXQueryExpression(), xmlQuery.getPassing(), rp);
			} catch (RuntimeException e) {
				if (e.getCause() instanceof XPathException) {
					throw (XPathException)e.getCause();
				}
				throw e;
			}
			if (rp != null) {
				XMLType.Type type = rp.type;
				if (type == null) {
					if (!emptyOnEmpty) {
						return null;
					}
					type = Type.CONTENT;
				}
				XMLType val = rp.concat.close();
				val.setType(rp.type);
				return val;
			}
			return xmlQuery.getXQueryExpression().createXMLType(result.iter, emptyOnEmpty);
		} catch (Exception e) {
			 throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30333, e.getMessage()));
		} finally {
			if (result != null) {
				result.close();
			}
		}
	}
	
	private Object evaluateXMLSerialize(XMLSerializeImpl xs)
			throws Exception {
		XMLType value = (XMLType) internalEvaluate(xs.getExpression());
		if (value == null) {
			return null;
		}
		try {
			if (!xs.getDocument()) {
				return XMLSystemFunctions.serialize(xs, value);
			}
			if (value.getType() == Type.UNKNOWN) {
				Type type = StringToSQLXMLTransform.isXml(value.getCharacterStream());
				value.setType(type);
			}
			if (value.getType() == Type.DOCUMENT || value.getType() == Type.ELEMENT) {
				return XMLSystemFunctions.serialize(xs, value);
			}
		} catch (Exception e) {
			 throw new TeiidClientException(e);
		}
		throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30336));
	}

	/**
	 * Taken from TextLine
	 */
	private static ValueExtractor<NameValuePair<Object>> defaultExtractor = new ValueExtractor<NameValuePair<Object>>() {
		@Override
        public Object getValue(NameValuePair<Object> t) {
			return t.value;
		}
	};

	/**
     * Taken from TextLine
     */
	private <T> String[] evaluate(final List<T> values, ValueExtractor<T> valueExtractor, TextLineImpl textLine) throws Exception {
        Character delimeter = textLine.getDelimiter();
        if (delimeter == null) {
            delimeter = Character.valueOf(',');
        }
        String delim = String.valueOf(delimeter.charValue());
        Character quote = textLine.getQuote();
        String quoteStr = null;     
        if (quote == null) {
            quoteStr = "\""; //$NON-NLS-1$
        } else {
            quoteStr = String.valueOf(quote);
        }
        String doubleQuote = quoteStr + quoteStr;
        ArrayList<String> result = new ArrayList<String>();
        DefaultDataTypeManager dataTypeManager = DefaultDataTypeManager.getInstance(textLine.getTeiidVersion());
        for (Iterator<T> iterator = values.iterator(); iterator.hasNext();) {
            T t = iterator.next();
            String text = (String)dataTypeManager.transformValue(
                                                                                      valueExtractor.getValue(t),
                                                                                      DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass());
            if (text == null) {
                continue;
            }
            result.add(quoteStr);
            result.add(StringUtil.replaceAll(text, quoteStr, doubleQuote));
            result.add(quoteStr);
            if (iterator.hasNext()) {
                result.add(delim);
            }           
        }
        result.add(textLine.getLineEnding());
        return result.toArray(new String[result.size()]);
    }

	private Object evaluateTextLine(TextLineImpl function) throws Exception {
		List<DerivedColumnImpl> args = function.getExpressions();
		Evaluator.NameValuePair<Object>[] nameValuePairs = getNameValuePairs(args, true, true);
		
		try {
			return new ArrayImpl(function.getTeiidVersion(), evaluate(Arrays.asList(nameValuePairs), defaultExtractor, function));
		} catch (Exception e) {
			 throw new TeiidClientException(e);
		}
	}

	private Object evaluateXMLForest(XMLForestImpl function)
			throws Exception {
		List<DerivedColumnImpl> args = function.getArgs();
		Evaluator.NameValuePair<Object>[] nameValuePairs = getNameValuePairs(args, true, true); 
			
		try {
			return XMLSystemFunctions.xmlForest(namespaces(function.getNamespaces()), nameValuePairs);
		} catch (Exception e) {
			 throw new TeiidClientException(e);
		}
	}
	
	private Object evaluateJSONObject(JSONObjectImpl function, JSONBuilder builder)
			throws Exception {
		List<DerivedColumnImpl> args = function.getArgs();
		Evaluator.NameValuePair<Object>[] nameValuePairs = getNameValuePairs(args, false, false);
		boolean returnValue = false;
		try {
			if (builder == null) {
				returnValue = true;
				//preevaluate subqueries to prevent blocked exceptions
				for (BaseSubqueryContainer<?> container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(function)) {
					evaluateSubquery(container);
				}
				builder = new JSONBuilder(function.getTeiidVersion());
			}
			builder.start(false);
			for (NameValuePair<Object> nameValuePair : nameValuePairs) {
				addValue(builder, nameValuePair.name, nameValuePair.value);
			}
			builder.end(false);
			if (returnValue) {
				ClobType result = builder.close();
				builder = null;
				return result;
			}
			return null;
		} catch (Exception e) {
			throw new TeiidClientException(e);
		} finally {
			if (returnValue && builder != null) {
				builder.remove();
			}
		}
	}

	private void addValue(JSONBuilder builder,
			String name, Object value)
			throws Exception {
		try {
			if (value instanceof JSONObjectImpl) {
				builder.startValue(name);
				evaluateJSONObject((JSONObjectImpl)value, builder);
				return;
			}
			if (value instanceof FunctionImpl) {
				FunctionImpl f = (FunctionImpl)value;
				if (FunctionLibrary.FunctionName.JSONARRAY.equalsIgnoreCase(f.getName())) {
					builder.startValue(name);
					jsonArray(f, f.getArgs(), builder, this);
					return;
				}
			}
			builder.addValue(name, internalEvaluate((BaseExpression)value));
		} catch (Exception e) {
			throw e;
		}
	}
	
	/**
	 * @param f
	 * @param vals
	 * @param builder
	 * @param eval
	 * @return json clob
	 * @throws Exception
	 */
	public static ClobType jsonArray(FunctionImpl f, Object[] vals, JSONBuilder builder, Evaluator eval) throws Exception {
		boolean returnValue = false;
		try {
			if (builder == null) {
				returnValue = true;
				if (eval != null) {
					//preevaluate subqueries to prevent blocked exceptions
					for (BaseSubqueryContainer<?> container : ValueIteratorProviderCollectorVisitorImpl.getValueIteratorProviders(f)) {
						eval.evaluateSubquery(container);
					}
				}
				builder = new JSONBuilder(f.getTeiidVersion());
			}
			builder.start(true);
			for (Object object : vals) {
				if (eval != null) {
					eval.addValue(builder, null, object);
				} else {
					builder.addValue(object);
				}
			}
			builder.end(true);
			if (returnValue) {
				ClobType result = builder.close();
				builder = null;
				return result;
			}
			return null;
		} finally {
			if (returnValue && builder != null) {
				builder.remove();
			}
		}
	}

	private Object evaluateXMLElement(XMLElementImpl function)
			throws Exception {
		List<BaseExpression> content = function.getContent();
		List<Object> values = new ArrayList<Object>(content.size());
		for (BaseExpression exp : content) {
			values.add(internalEvaluate(exp));
		}
		try {
			Evaluator.NameValuePair<Object>[] attributes = null;
			if (function.getAttributes() != null) {
				attributes = getNameValuePairs(function.getAttributes().getArgs(), true, true);
			}
			return XMLSystemFunctions.xmlElement(function.getName(), namespaces(function.getNamespaces()), attributes, values);
		} catch (Exception e) {
			throw new TeiidClientException(e);
		}
	}
	
	private Result evaluateXQuery(SaxonXQueryExpression xquery, List<DerivedColumnImpl> cols, RowProcessor processor) 
	throws Exception {
		HashMap<String, Object> parameters = new HashMap<String, Object>();
		Object contextItem = evaluateParameters(cols, parameters);
		return XQueryEvaluator.evaluateXQuery(xquery, contextItem, parameters, processor);
	}

	/**
	 * Evaluate the parameters and return the context item if it exists
	 * @param cols
	 * @param parameters 
	 * @return context item
	 * @throws Exception 
	 */
	public Object evaluateParameters(List<DerivedColumnImpl> cols,
			Map<String, Object> parameters)
			throws Exception {
		Object contextItem = null;
		for (DerivedColumnImpl passing : cols) {
			Object value = evaluateParameter(passing);
			if (passing.getAlias() == null) {
				contextItem = value;
			} else {
				parameters.put(passing.getAlias(), value);
			}
		}
		return contextItem;
	}

	private Object evaluateParameter(DerivedColumnImpl passing)
			throws Exception {
		if (passing.getExpression() instanceof FunctionImpl) {
			FunctionImpl f = (FunctionImpl)passing.getExpression();
			//narrow optimization of json based documents to allow for lower overhead streaming
			if (f.getName().equalsIgnoreCase(SourceSystemFunctions.JSONTOXML)) {
				String rootName = (String)this.evaluate(f.getArg(0));
				Object lob = this.evaluate(f.getArg(1));
				if (rootName == null || lob == null) {
					return null;
				}
				try {
					if (lob instanceof Blob) {
						return XMLSystemFunctions.jsonToXml(rootName, (Blob)lob, true);
					}
					return XMLSystemFunctions.jsonToXml(rootName, (Clob)lob, true);
				} catch (Exception e) {
					throw new TeiidClientException(e, Messages.gs(Messages.TEIID.TEIID30384, f.getFunctionDescriptor().getName()));
				}
			}
		} else if (passing.getExpression() instanceof XMLParseImpl) {
			XMLParseImpl xmlParse = (XMLParseImpl)passing.getExpression();
			xmlParse.setWellFormed(true);
		}
		Object value = this.evaluate(passing.getExpression());
		return value;
	}

	private Evaluator.NameValuePair<Object>[] getNameValuePairs(List<DerivedColumnImpl> args, boolean xmlNames, boolean eval)
			throws Exception {
		Evaluator.NameValuePair<Object>[] nameValuePairs = new Evaluator.NameValuePair[args.size()];
		for (int i = 0; i < args.size(); i++) {
			DerivedColumnImpl symbol = args.get(i);
			String name = symbol.getAlias();
			BaseExpression ex = symbol.getExpression();
			if (name == null && ex instanceof ElementSymbolImpl) {
				name = ((ElementSymbolImpl)ex).getShortName();
				if (xmlNames) {
					name = XMLSystemFunctions.escapeName(name, true);
				}
			}
			if (!xmlNames && name == null) {
				name = "expr" + (i+1); //$NON-NLS-1$
			}
			nameValuePairs[i] = new Evaluator.NameValuePair<Object>(name, eval?internalEvaluate(ex):ex);
		}
		return nameValuePairs;
	}
	
	private Evaluator.NameValuePair<String>[] namespaces(XMLNamespacesImpl namespaces) {
		if (namespaces == null) {
			return null;
		}
	    List<NamespaceItem> args = namespaces.getNamespaceItems();
	    Evaluator.NameValuePair<String>[] nameValuePairs = new Evaluator.NameValuePair[args.size()];
	    for(int i=0; i < args.size(); i++) {
	    	NamespaceItem item = args.get(i);
	    	nameValuePairs[i] = new Evaluator.NameValuePair<String>(item.getPrefix(), item.getUri());
	    } 
	    return nameValuePairs;
	}
	
	private Object evaluate(CaseExpressionImpl expr)
	throws Exception {
	    Object exprVal = internalEvaluate(expr.getExpression());
	    for (int i = 0; i < expr.getWhenCount(); i++) {
	        Object intEvObj = internalEvaluate(expr.getWhenExpression(i));
	        if (intEvObj != null && exprVal != null && intEvObj.equals(exprVal)) {
	            return internalEvaluate(expr.getThenExpression(i));
	        }
	    }
	    if (expr.getElseExpression() != null) {
	        return internalEvaluate(expr.getElseExpression());
	    }
	    return null;
	}
	
	private Object evaluate(SearchedCaseExpressionImpl expr)
	throws Exception {
	    for (int i = 0; i < expr.getWhenCount(); i++) {
            if (evaluate(expr.getWhenCriteria(i))) {
                return internalEvaluate(expr.getThenExpression(i));
            }
	    }
	    if (expr.getElseExpression() != null) {
	        return internalEvaluate(expr.getElseExpression());
	    }
	    return null;
	}
	
	private Object evaluate(FunctionImpl function)
		throws Exception {
	
	    // Get function based on resolved function info
	    TCFunctionDescriptor fd = function.getFunctionDescriptor();
	    
		// Evaluate args
		BaseExpression[] args = function.getArgs();
	    Object[] values = null;
	    int start = 0;
	    
	    if (fd.requiresContext()) {
			values = new Object[args.length+1];
	        values[0] = commandContext;
	        start = 1;
	    }
	    else {
	        values = new Object[args.length];
	    }
	    
	    for(int i=0; i < args.length; i++) {
	        values[i+start] = internalEvaluate(args[i]);
	    }            
	    
	    if (fd.getPushdown() == PushDown.MUST_PUSHDOWN) {
	    	try {
				return evaluatePushdown(function, values);
			} catch (Exception e) {
				throw new TeiidClientException(e);
			}
	    }
	    
	    // Check for special lookup function
	    if(FunctionLibrary.FunctionName.LOOKUP.equalsIgnoreCase(function.getName())) {
	
	        String codeTableName = (String) values[0];
	        String returnElementName = (String) values[1];
	        String keyElementName = (String) values[2];

	        /*
	         * Full-version uses the processor to lookup the value which
	         * requires a load more classes that seem unnecessary for
	         * just a validation and 1 function.
	         */
	        if (codeTableName != null && returnElementName != null && keyElementName != null)
	            return codeTableName + SQLConstants.Tokens.DOT + returnElementName + SQLConstants.Tokens.DOT + keyElementName;
	        else
				throw new TeiidClientException();
	    }

		// Execute function
		return fd.invokeFunction(values, commandContext, null);
	}
	
	protected Object evaluatePushdown(FunctionImpl function,
			Object[] values) throws Exception {
		throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30341, function.getFunctionDescriptor().getFullName()));
	}

	private Object evaluate(ScalarSubqueryImpl scalarSubquery)
	    throws Exception {
		
	    Object result = null;
        ValueIterator valueIter;
		try {
			valueIter = evaluateSubquery(scalarSubquery);
		} catch (Exception e) {
			 throw new TeiidClientException(e);
		}
	    if(valueIter.hasNext()) {
	        result = valueIter.next();
	        if(valueIter.hasNext()) {
	            // The subquery should be scalar, but has produced
	            // more than one result value - this is an exception case
	             throw new TeiidClientException(Messages.gs(Messages.TEIID.TEIID30345, scalarSubquery.getCommand()));
	        }
	    }
	    return result;
	}
	
	/**
	 * @param container
	 * @param tuple
	 * @return
	 * @throws TeiidProcessingException
	 * @throws BlockedException
	 * @throws TeiidComponentException
	 */
	protected ValueIterator evaluateSubquery(BaseSubqueryContainer<?> container) 
	throws Exception {
		throw new UnsupportedOperationException("Subquery evaluation not possible with a base Evaluator"); //$NON-NLS-1$
	}
}
