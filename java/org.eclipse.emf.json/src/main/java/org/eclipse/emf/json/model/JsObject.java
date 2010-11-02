/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.eclipse.emf.json.model;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.EMap;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Js Object</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.emf.json.model.JsObject#getUnmatched <em>Unmatched</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.json.model.JsonPackage#getJsObject()
 * @model
 * @generated
 */
public interface JsObject extends EObject {
	/**
	 * Returns the value of the '<em><b>Unmatched</b></em>' map.
	 * The key is of type {@link java.lang.String},
	 * and the value is of type {@link java.lang.Object},
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Unmatched</em>' map isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Unmatched</em>' map.
	 * @see org.eclipse.emf.json.model.JsonPackage#getJsObject_Unmatched()
	 * @model mapType="org.eclipse.emf.json.model.EStringToAnySimpleTypeMap<org.eclipse.emf.ecore.EString, org.eclipse.emf.json.model.AnySimpleType>"
	 *        annotation="JsonMetadata wildcard='true'"
	 * @generated
	 */
	EMap<String, Object> getUnmatched();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model dataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	Object get(String key);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 * @generated
	 */
	EList<String> getFieldNames();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model valueDataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	JsObject with(String key, Object value);

} // JsObject
